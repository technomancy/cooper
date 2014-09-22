#lang racket/gui


;;; types

(struct card (name background buttons) #:prefab)

(struct button (corners action) #:prefab)

(struct stack (name cards) #:prefab)

(struct state (card stack mode mouse) #:prefab)

(struct mode (name color submodes onclick onrelease paint next) #:prefab)

(define (current-card state)
  (hash-ref (stack-cards (state-stack state)) (state-card state)))


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

(define (card-canvas% now)
  (class canvas%
    (define/override (on-event event)
      (when (unbox debug)
        (printf "state: ~s~n" (unbox now)))
      (when (eq? 'left-down (send event get-event-type))
        (let ([onclick (mode-onclick (state-mode (unbox now)))])
          (and onclick (swap! now onclick this event))
          (send this on-paint)))
      (when (eq? 'left-up (send event get-event-type))
        (let ([onrelease (mode-onrelease (state-mode (unbox now)))])
          (and onrelease (swap! now onrelease this event))
          (send this on-paint))
        (swap! now update 'mouse (lambda (_) #f)))
      (when (eq? 'right-down (send event get-event-type))
        (swap! now update 'mode next-mode)
        (send this on-paint)))
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

(define (find-card stack card-name)
  (hash-ref (stack-cards stack) card-name))


;;; normal mode

(define (button-hit? button event)
  (match (button-corners button)
    [(list left top right bottom)
     (and (<= left (send event get-x) right)
          (<= top (send event get-y) bottom))]))

(define (normal-click st canvas event)
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

(define (button-click state canvas event)
  (update state 'mouse (lambda (_) event)))

(define (make-button-corners down-event up-event)
  (list (min (send down-event get-x) (send up-event get-x))
        (min (send down-event get-y) (send up-event get-y))
        (max (send down-event get-x) (send up-event get-x))
        (max (send down-event get-y) (send up-event get-y))))

(define (button-release state canvas event)
  (let ([corners (make-button-corners (state-mouse state) event)]
        [target (get-text-from-user "card" "which card?")])
    (if target
        (update state 'stack
                update 'cards hash-update (state-card state)
                update 'buttons (flip cons) (button corners target))
        state)))

(define (button-paint state dc canvas)
  (send dc set-brush "white" 'transparent)
  (send dc set-pen "black" 1 'long-dash)
  (for ([button (card-buttons (current-card state))])
    (match (button-corners button)
      [(list left top right bottom)
       (send dc draw-rectangle left top (- right left) (- bottom top))])))


;;; draw mode

(define (draw-click state canvas event)
  (update state 'mouse (lambda (_) event)))

(define (draw-release state canvas event)
  (let ([old-event (state-mouse state)])
    (update state 'stack
            update 'cards hash-update (state-card state)
            update 'background (flip cons) (list 'draw-line
                                                 (send old-event get-x)
                                                 (send old-event get-y)
                                                 (send event get-x)
                                                 (send event get-y)))))

(define (draw-paint state dc canvas)
  #f)


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

(define (load-stack filename)
  (stack (stack-filename->name filename)
         (for/hash [(b (call-with-input-file filename read))]
           (match b
             [(list name background buttons)
              (values name (card name background
                                 (map (curry apply button) buttons)))]))))

(define (main stack-name . args)
  (let* ([main-stack (load-stack stack-name)]
         [first-card (first (hash-keys (stack-cards main-stack)))]
         [now (box (state first-card main-stack (hash-ref modes "normal") #f))]
         [frame (new frame% [label stack-name])]
         [canvas (new (card-canvas% now) [parent frame]
                      [paint-callback (curry paint now)])])
    (send frame show #t)
    now))

(define modes `#hash(("normal" . ,(mode "normal" "white" '()
                                        normal-click #f #f "buttons"))
                     ("buttons" . ,(mode "buttons" "blue" '()
                                         button-click button-release
                                         button-paint "draw"))
                     ("draw" . ,(mode "draw" "red" '()
                                      draw-click draw-release draw-paint
                                      "cards"))
                     ("cards" . ,(mode "cards" "green" '()
                                       cards-click #f #f "normal"))))

;; for quick testing
;; (main "mystack.rkt")
