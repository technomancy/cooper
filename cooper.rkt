#lang racket/gui


;;; types

(struct card (name background buttons) #:prefab)

(struct button (corners action) #:prefab)

(struct stack (name cards width height) #:prefab)

(struct state (card stack mode mouse-down mouse-last) #:prefab)

(struct mode (name color submodes onclick onrelease onmove paint next)
        #:prefab)


;;; helpers

;; TODO: figure out how to do this without eval
(define (update x field f . args)
  (let-values ([(type _) (struct-info x)])
    (let* ([function-name (format "~s-~s" (object-name type) field)]
           [field-function (eval (string->symbol function-name))])
      (eval (list 'struct-copy (object-name type) x
                  [list field `(quote ,(apply f (field-function x) args))])))))

;; from rackjure
(define (swap! box f . args)
  (let [(old-value (unbox box))]
    (box-cas! box old-value (apply f old-value args))))

(define (hash-update h k f . args)
  (let ([old-value (hash-ref h k #f)])
    (hash-set h k (apply f old-value args))))

(define (flip f)
  (lambda args (apply f (reverse args))))


;;; general

(define debug (box #f))

(define (next-mode mode)
  (hash-ref modes (mode-next mode)))

(define (handle-key now event)
  (when (and (send event get-control-down)
             (equal? (send event get-key-code) #\s))
    (let ([filename (put-file "Save to:")])
      (when filename
        (call-with-output-file filename (curry write (unbox now)))))))

(define (handle-mouse now canvas event)
  (case (send event get-event-type)
    ['left-down
     (swap! now update 'mouse-down (lambda (_) event))
     (let ([onclick (mode-onclick (state-mode (unbox now)))])
       (and onclick (swap! now onclick canvas event))
       (send canvas on-paint))]
    ['left-up
     (let ([down (state-mouse-down (unbox now))]
           [last (state-mouse-last (unbox now))]
           [onrelease (mode-onrelease (state-mode (unbox now)))])
       (swap! now update 'mouse-down (lambda (_) #f))
       (swap! now update 'mouse-last (lambda (_) #f))
       (and onrelease (swap! now onrelease canvas event down last))
       (send canvas on-paint))]
    ['motion
     (let ([onmove (mode-onmove (state-mode (unbox now)))])
       (and onmove (swap! now onmove canvas event)))]
    ['right-down (swap! now update 'mode next-mode)
                 (send canvas on-paint)]))

(define (card-canvas% now)
  (class canvas%
    (define/override (on-char event)
      (handle-key now event))
    (define/override (on-event event)
      (when (unbox debug)
        (printf "state: ~s~n" (unbox now)))
      (handle-mouse now this event))
    (super-new)))

(define (mode-border mode dc canvas)
  (send dc set-pen (mode-color mode) 2 'solid)
  (let-values ([(width height) (send canvas get-client-size)])
    (send dc draw-rectangle 0 0 width height)))

(define (paint now canvas dc)
  (send dc clear)
  (send dc set-brush "white" 'solid)
  (mode-border (state-mode (unbox now)) dc canvas)
  (send dc set-pen "black" 1 'solid) ;; default
  (for [(step (card-background (current-card (unbox now))))]
    (apply dynamic-send dc step))
  (let ([painter (mode-paint (state-mode (unbox now)))])
    (when painter
      (painter (unbox now) dc canvas))))

(define (current-card state)
  (hash-ref (stack-cards (state-stack state)) (state-card state)))


;;; explore mode

(define (button-hit? button event)
  (match (button-corners button)
    [(list left top right bottom)
     (and (<= left (send event get-x) right)
          (<= top (send event get-y) bottom))]))

(define (explore-click st canvas event)
  (when (unbox debug)
    (printf "Click: ~s ~s~n" (send event get-x) (send event get-y)))
  (or (for/or [(button (card-buttons (current-card st)))]
        (if (button-hit? button event)
            (let [(action (button-action button))]
              (if (string? action)
                  (struct-copy state st [card action])
                  (action st)))
            #f)) st))


;;; buttons mode

(define (make-button-corners down-event up-event)
  (list (min (send down-event get-x) (send up-event get-x))
        (min (send down-event get-y) (send up-event get-y))
        (max (send down-event get-x) (send up-event get-x))
        (max (send down-event get-y) (send up-event get-y))))

(define (existing-card? state card-name)
  (member card-name (hash-keys (stack-cards (state-stack state)))))

(define (button-release state canvas event down last)
  (if (and down last)
      (let ([corners (make-button-corners down last)]
            [target (get-text-from-user "card" "which card?"
                                        #:validate (curry existing-card? state))])
        (if target
            (update state 'stack
                    update 'cards hash-update (state-card state)
                    update 'buttons (flip cons) (button corners target))
            state))
      state))

(define (render-button button dc)
  (match (button-corners button)
    [(list left top right bottom)
     (send dc draw-rectangle
           (min left right) (min top bottom)
           (- (max left right) (min left right))
           (- (max top bottom) (min top bottom)))]))

(define (button-paint state dc canvas)
  (send dc set-brush "white" 'transparent)
  (send dc set-pen "black" 1 'long-dash)
  (for ([button (card-buttons (current-card state))])
    (render-button button dc))
  (let ([down (state-mouse-down state)]
        [last (state-mouse-last state)])
    ;; TODO: this hasn't made it into the state record yet
    (when (and down last)
      (render-button (button (list (send down get-x) (send down get-y)
                                   (send last get-x) (send last get-y))
                             "dummy") dc))))

(define (button-move state canvas event)
  (if (state-mouse-down state)
      (let ([state (update state 'mouse-last (lambda (_) event))])
        (send canvas on-paint)
        state)
      state))


;;; draw mode

(define (draw-release state canvas event down last)
  (update state 'stack
          update 'cards hash-update (state-card state)
          update 'background (flip cons) (list 'draw-line
                                               (send down get-x)
                                               (send down get-y)
                                               (send last get-x)
                                               (send last get-y))))

(define (draw-paint state dc canvas)
  (send dc set-pen "black" 1 'solid)
  (let ([down (state-mouse-down state)]
        [last (state-mouse-last state)])
    (when (and down last)
      (send dc draw-line
            (send down get-x) (send down get-y)
            (send last get-x) (send last get-y)))))

(define (draw-move state canvas event)
  (let ([state (update state 'mouse-last (lambda (_) event))])
    (when (state-mouse-down state)
      (send canvas on-paint))
    state))


;;; cards mode

(define (cards-click state canvas event)
  (let ([card-name (get-text-from-user "card" "New card name:")])
    (if card-name
        (update state 'stack
                update 'cards hash-set card-name (card card-name '() '()))
        state)))


;;; loading

(define (stack-filename->name filename)
  (first (string-split (path->string (file-name-from-path filename)) ".")))

(define (main filename . args)
  (let* ([stack (call-with-input-file filename read)]
         [first-card (first (hash-keys (stack-cards stack)))]
         [now (box (state first-card stack (hash-ref modes "explore") #f #f))]
         [frame (new frame% [label filename]
                     [width (stack-width stack)] [height (stack-height stack)])]
         [canvas (new (card-canvas% now) [parent frame]
                      [paint-callback (curry paint now)])])
    (send frame show #t)
    now))

(define modes `#hash(("explore" . ,(mode "explore" "white" '()
                                         explore-click #f #f #f "buttons"))
                     ("buttons" . ,(mode "buttons" "blue" '()
                                         #f button-release
                                         button-move button-paint "draw"))
                     ("draw" . ,(mode "draw" "red" '()
                                      #f draw-release draw-move draw-paint
                                      "cards"))
                     ("cards" . ,(mode "cards" "green" '()
                                       cards-click #f #f #f "explore"))))

;; for quick testing
;; (main "mystack.rkt")
