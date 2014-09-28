#lang racket/gui


;;; types

(struct card (name background buttons events) #:prefab)

(struct button (corners action visible? name) #:prefab)

(struct stack (name cards width height) #:prefab)

(struct state (card stack mode mouse last-mouse) #:prefab)

(struct mode (name color submodes onclick onrelease onmove paint next)
        #:prefab)

(define (current-card state)
  (hash-ref (stack-cards (state-stack state)) (state-card state)))


;;; helpers

;; TODO: use functional struct macro
(define (update x field f . args)
  (let-values ([(type _) (struct-info x)])
    (let* ([function-name (format "~s-~s" (object-name type) field)]
           [field-function (eval (string->symbol function-name))])
      (eval (list 'struct-copy (object-name type) x
                  [list field `(quote ,(apply f (field-function x) args))])))))

;; not actually atomic--box-cas! doesn't work on impersonated boxes,
;; and we have a semaphore around handlers anyway.
(define (swap! box f . args)
  (set-box! box (apply f (unbox box) args)))

(define (hash-update h k f . args)
  (let ([old-value (hash-ref h k #f)])
    (hash-set h k (apply f old-value args))))

(define (flip f)
  (lambda args (apply f (reverse args))))

(define (replace lst old new)
  (map (lambda (x) (if (equal? x old) new x)) lst))


;;; imperative bits

(define debug (box #f))

(define (next-mode mode)
  (hash-ref modes (mode-next mode)))

(define (handle-key now canvas event)
  (when (send event get-control-down)
    (case (send event get-key-code)
      [(#\s) (let ([filename (put-file "Save to:")])
               (when filename
                 (call-with-output-file filename
                   (curry write (state-stack (unbox now))))))]
      [(#\l) (let ([filename (get-file "Load:")])
               (when filename
                 (swap! now update 'stack
                        (lambda (_) (call-with-input-file filename read)))
                 ;; TODO: not all stacks have a zero card?
                 (swap! now update 'card (lambda (_) "zero"))))]
      [(#\c) (swap! now update 'stack update 'cards
                    hash-update (state-card (unbox now))
                    update 'background (lambda (_) '()))]
      [(#\b) (swap! now update 'stack update 'cards
                    hash-update (state-card (unbox now))
                    update 'buttons (lambda (_) '()))]
      ;; TODO: need zero-enter here
      [(#\0) (swap! now update 'card (lambda (_) "zero"))])
    (send canvas refresh)))

(define (handle-mouse now canvas event)
  (case (send event get-event-type)
    ;; TODO: clauses should begin with a list, not quote
    ['left-down
     (swap! now update 'mouse hash-set 'down event)
     (swap! now update 'mouse hash-set 'last event)
     (swap! now update 'mouse hash-set 'at (current-milliseconds))
     (let ([onclick (mode-onclick (state-mode (unbox now)))])
       (and onclick (swap! now onclick canvas event))
       (send canvas refresh))]
    ['left-up
     (let ([mouse (state-mouse (unbox now))]
           [onrelease (mode-onrelease (state-mode (unbox now)))])
       (swap! now update 'mouse (lambda (_) (hash)))
       (swap! now update 'last-mouse (lambda (_) mouse))
       (and onrelease (swap! now onrelease canvas event mouse))
       (send canvas refresh))]
    ['motion
     (let ([onmove (mode-onmove (state-mode (unbox now)))])
       (and onmove (swap! now onmove canvas event))
       (swap! now update 'mouse hash-set 'last event))]
    ['right-down (swap! now update 'mode next-mode)
                 (swap! now update 'mouse (lambda (_) (hash)))
                 (send canvas refresh)]))

(define (card-canvas% now semaphore)
  (class canvas%
    (when (unbox debug)
      (printf "state: ~s~n" (unbox now)))
    (define/override (on-char event)
      (call-with-semaphore semaphore handle-key
                           (lambda () #f) now this event))
    (define/override (on-event event)
      (call-with-semaphore semaphore handle-mouse
                           (lambda () #f) now this event))
    (super-new)))

(define (mode-border mode dc canvas)
  (send dc set-pen (mode-color mode) 2 'solid)
  (let-values ([(width height) (send canvas get-client-size)])
    (send dc draw-rectangle 0 0 width height)))

(define (paint now canvas dc)
  (send dc clear)
  (send dc set-brush "white" 'solid)
  (mode-border (state-mode (unbox now)) dc canvas)
  (send dc set-pen "black" 1 'solid) ; default
  (send dc set-smoothing 'unsmoothed)
  (for [(step (card-background (current-card (unbox now))))]
    (apply dynamic-send dc step))
  (for [(button (card-buttons (current-card (unbox now))))]
    (render-button button dc #f))
  (let ([painter (mode-paint (state-mode (unbox now)))])
    (when painter
      (painter (unbox now) dc canvas))))


;;; explore mode

(define (button-hit? event button)
  (match (button-corners button)
    [(list left top right bottom)
     (and (<= left (send event get-x) right)
          (<= top (send event get-y) bottom))]))

(define (explore-click state canvas event)
  (or (for/or [(button (card-buttons (current-card state)))]
        (if (button-hit? event button)
            (if (or (hash-ref (stack-cards (state-stack state))
                              (button-action button) #f)
                    (procedure? (eval (button-action button))))
                (let* ([action (button-action button)]
                       [leave (hash-ref (card-events (current-card state))
                                        'leave 'identity)]
                       [action (if (string? action)
                                   (lambda (state)
                                     (update state 'card (lambda (_) action)))
                                   action)]
                       [enter (hash-ref (card-events (current-card state))
                                        'enter 'identity)])
                  ((apply compose (map eval (list enter action leave))) state))
                (if (equal? (message-box "new card?"
                                         (format "Unknown card ~s; create it?"
                                                 (button-action button))
                                         #f '[ok-cancel])'ok)
                    (update state 'stack
                            update 'cards hash-set (button-action button)
                            (card (button-action button)
                                  '() '() (hash)))
                    state))
            #f)) state))


;;; buttons mode

(define (make-button-corners down-event up-event)
  (list (min (send down-event get-x) (send up-event get-x))
        (min (send down-event get-y) (send up-event get-y))
        (max (send down-event get-x) (send up-event get-x))
        (max (send down-event get-y) (send up-event get-y))))

(define (existing-card? state card-name)
  (member card-name (hash-keys (stack-cards (state-stack state)))))

(define double-click-threshold 500)

(define (double-click? mouse last-mouse)
  (> double-click-threshold (- (hash-ref mouse 'at)
                               (hash-ref last-mouse 'at 0))))

(define (button-editor-frame% sem)
  (class frame%
    (super-new)
    (define (on-close)
      (semaphore-post sem))
    (augment on-close)))

(define (button-edit-window button)
  (let* ([sem (make-semaphore 0)]
         [editor (new text%)]
         ;; TODO: turn read forms back into string
         [snip (make-object string-snip% (button-action button))]
         [frame (new (button-editor-frame% sem) [label "Button Edit"]
                     [width 500] [height 500])]
         [canvas (new editor-canvas% [parent frame])]
         [mb (new menu-bar% [parent frame])]
         [m-edit (new menu% [label "Edit"] [parent mb])])
    (append-editor-operation-menu-items m-edit #t)
    (send canvas set-editor editor)
    ;; TODO: this does nothing
    (send editor change-style (make-object style-delta% 'change-family 'modern)
          'start 'end)
    ;; TODO: visible/name
    ;; TODO: add ok/cancel buttons
    ;; TODO: add select menu for existing cards
    (send frame show #t)
    (send editor insert snip)
    (define (blocker) ; probably a better way to do this
      (when (not (semaphore-try-wait? sem))
        (sleep/yield 0.1)
        (blocker)))
    (blocker)
    (send editor get-flattened-text)))

(define (button-edit state target-button)
  (let* ([input (button-edit-window target-button)]
         [action (if (hash-ref (stack-cards (state-stack state)) input #f)
                     input
                     ;; TODO: check for readable input here
                     (read-string input))]
         [new-button (update target-button 'action (lambda (_) action))])
    (update state 'stack update 'cards
            hash-update (state-card state) update 'buttons
            replace target-button new-button)))

(define (button-click state canvas event)
  (let ([target-button (findf (curry button-hit? event)
                              (card-buttons (current-card state)))])
    (if target-button
        (let ([state (update state 'mouse hash-set 'target-button target-button)])
          (if (double-click? (state-mouse state) (state-last-mouse state))
              (button-edit state target-button)
              state))
        state)))

(define (button-release state canvas event mouse)
  (if (not (hash-ref mouse 'target-button #f))
      (let ([corners (make-button-corners (hash-ref mouse 'down)
                                          (hash-ref mouse 'last))])
        (update state 'stack
                update 'cards hash-update (state-card state)
                update 'buttons (flip cons) (button corners "" #f #f)))
      state))

(define (render-button button dc render-invisible?)
  (if (button-visible? button)
      (send dc set-pen "black" 1 'solid)
      (send dc set-pen "black" 1 'long-dash))
  (match (button-corners button)
    [(list left top right bottom)
     (when (or render-invisible? (button-visible? button))
       (send dc draw-rectangle
             (min left right) (min top bottom)
             (- (max left right) (min left right))
             (- (max top bottom) (min top bottom))))
     (when (and (button-visible? button) (button-name button))
       (send dc draw-text (button-name button) (+ 6 left) (+ 3 top)))]))

(define (button-paint state dc canvas)
  (send dc set-brush "white" 'transparent)
  (send dc set-smoothing 'unsmoothed)
  (for ([button (card-buttons (current-card state))])
    (render-button button dc #t))
  ;; if a button is currently being created
  (let ([down (dict-ref (state-mouse state) 'down #f)]
        [last (dict-ref (state-mouse state) 'last #f)])
    (when (and down last (not (dict-ref (state-mouse state) 'target-button #f)))
      (render-button (button (list (send down get-x) (send down get-y)
                                   (send last get-x) (send last get-y))
                             "" #f #f) dc #t))))

(define button-resize-threshold 10)

(define (button-corner-deltas corners click-x click-y dx dy)
  ;; very repetitive, room for improvement here
  (list (if (> (- (third corners) click-x) button-resize-threshold) dx 0)
        (if (> (- (fourth corners) click-y) button-resize-threshold) dy 0)
        (if (> (- click-x (first corners)) button-resize-threshold) dx 0)
        (if (> (- click-y (second corners)) button-resize-threshold) dy 0)))

(define (button-new-corners old-corners last-mouse new-mouse)
  (let* ([start-x (send last-mouse get-x)]
         [start-y (send last-mouse get-y)]
         [delta-x (- (send new-mouse get-x) start-x)]
         [delta-y (- (send new-mouse get-y) start-y)])
    (map + (button-corner-deltas old-corners start-x start-y
                                 delta-x delta-y) old-corners)))

(define (button-drag target event state)
  (let* ([card-name (state-card state)]
         [last-mouse (hash-ref (state-mouse state) 'last)]
         [new-button (update target 'corners button-new-corners last-mouse event)]
         [state (update state 'mouse hash-set 'target-button new-button)]
         [state (update state 'mouse hash-set 'target-start-xy
                  (list (send event get-x) (send event get-y)))])
    (update state 'stack update 'cards
            hash-update card-name update 'buttons
            replace target new-button)))

(define (button-move state canvas event)
  (if (dict-ref (state-mouse state) 'down #f)
      (let* ([target-button (dict-ref (state-mouse state) 'target-button #f)]
             [state (if target-button
                       (button-drag target-button event state)
                       state)])
        (send canvas refresh)
        state)
      state))


;;; draw mode

(define (draw-release state canvas event mouse)
  (let ([down (hash-ref mouse 'down)]
        [last (hash-ref mouse 'last)])
    (update state 'stack
            update 'cards hash-update (state-card state)
            update 'background (flip cons) (list 'draw-line
                                                 (send down get-x)
                                                 (send down get-y)
                                                 (send last get-x)
                                                 (send last get-y)))))

(define (draw-paint state dc canvas)
  (send dc set-pen "black" 1 'solid)
  (let ([down (dict-ref (state-mouse state) 'down #f)]
        [last (dict-ref (state-mouse state) 'last #f)])
    (when (and down last)
      (send dc draw-line
            (send down get-x) (send down get-y)
            (send last get-x) (send last get-y)))))

(define (draw-move state canvas event)
  (when (dict-ref (state-mouse state) 'down #f)
    (send canvas refresh))
  state)


;;; first card

(define zero-button-x-offset 25)

(define zero-button-y-offset 30)

(define (zero-place-button card i buttons)
  (cons (button (list zero-button-x-offset (* zero-button-y-offset (add1 i))
                      ;; TODO: 200 is nonsense here
                      200 (- (* zero-button-y-offset (+ 2 i)) 5))
                (card-name card) #t (card-name card))
        buttons))

(define (zero-delete-card card state)
  (zero-enter (update state 'stack update 'cards hash-remove (card-name card))))

(define (zero-place-delete-button card i buttons)
  (cons (button (list (+ 220 zero-button-x-offset)
                      (* zero-button-y-offset (add1 i))
                      (+ 240 zero-button-x-offset)
                      (- (* zero-button-y-offset (+ 2 i)) 5))
                `(lambda (state) (zero-delete-card ,card state)) #t "x")
        buttons))

(define (zero-buttons stack card)
  (let ([jump-buttons (foldl zero-place-button '()
                             (hash-values (stack-cards stack))
                             (range (hash-count (stack-cards stack))))]
        [delete-buttons (foldl zero-place-delete-button '()
                               (hash-values (stack-cards stack))
                               (range (hash-count (stack-cards stack))))])
    (update card 'buttons (lambda (_) (append (list zero-new-card-button)
                                              jump-buttons delete-buttons)))))

(define (zero-enter state)
  (update state 'stack update 'cards hash-update (state-card state)
          (curry zero-buttons (state-stack state))))

(define (zero-new-card state)
  (let ([card-name (get-text-from-user "card" "New card name:")])
    (if card-name
        (zero-enter (update state 'stack
                            update 'cards hash-set card-name
                            (card card-name '() '() (hash))))
        state)))

(define zero-new-card-button (button '(0 0 25 30) 'zero-new-card #t "+"))

(define card-zero (card "zero" '() '() (hash "enter" 'zero-enter)))

(define card-one (card "one" '() '() (hash)))


;;; loading

(define (stack-filename->name filename)
  (first (string-split (path->string (file-name-from-path filename)) ".")))

(define (main [filename #f] . args)
  (let* ([stack (if (and filename (file-exists? filename))
                    (call-with-input-file filename read)
                    (stack (or filename "new") (hash "zero" card-zero
                                                     "one" card-one) 800 600))]
         [now (box (zero-enter (state "zero" stack (hash-ref modes "explore")
                                      (hash) (hash))))]
         [frame (new frame% [label (stack-name stack)]
                     [width (stack-width stack)] [height (stack-height stack)])]
         [canvas (new (card-canvas% now (make-semaphore 1)) [parent frame]
                      [paint-callback (curry paint now)])])
    (send frame show #t)
    now))

(define modes `#hash(("explore" . ,(mode "explore" "white" '()
                                         explore-click #f #f #f "buttons"))
                     ("buttons" . ,(mode "buttons" "blue" '()
                                         button-click button-release
                                         button-move button-paint "draw"))
                     ("draw" . ,(mode "draw" "red" '()
                                      #f draw-release draw-move draw-paint
                                      "explore"))))

(module+ main
  (main))
;; for quick testing
;; (main "mystack.rkt")
