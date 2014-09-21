#lang racket/gui


;;; types

;; TODO add card events: enter, leave, key, tick
(struct card (name background buttons) #:prefab)

(struct button (corners action) #:prefab)

(struct stack (name cards) #:prefab)

(struct state (card stack mode) #:prefab)

(struct mode (name color submodes onclick next) #:prefab)

(define modes `#hash(("normal" . ,(mode "normal" "white" '() #f "buttons"))
                     ("buttons" . ,(mode "buttons" "blue" '() #f "draw"))
                     ("draw" . ,(mode "draw" "red" '() #f "cards"))
                     ("cards" . ,(mode "cards" "green" '() #f "normal"))))


;;; helpers

;; TODO: figure out how to do this without eval
(define (update x field f . args)
  (let-values ([(type _) (struct-info x)])
    (let* ([function-name (format "~s-~s" (object-name type) field)]
           [field-function (eval (string->symbol function-name))])
      (eval (list 'struct-copy (object-name type) x
                  [list field (apply f (field-function x) args)])))))

;; from rackjure
(define (swap! box f . args)
  (let [(old-value (unbox box))]
    (box-cas! box old-value (apply f old-value args))))


;;; rendering

(define (next-mode mode)
  (hash-ref modes (mode-next mode)))

(define (card-canvas% now)
  (class canvas%
    (define/override (on-event event)
      (when (eq? 'left-down (send event get-event-type))
        (onclick this now event))
      (when (eq? 'right-down (send event get-event-type))
        (swap! now update 'mode next-mode)
        (send this on-paint)))
    (super-new)))

(define (paint now canvas dc)
  (send dc clear)
  (send dc set-brush "white" 'solid)
  (let ([mode (state-mode (unbox now))])
    (send dc set-pen (mode-color mode) 2 'solid)
    (let-values ([(width height) (send canvas get-client-size)])
      (send dc draw-rectangle 0 0 width height)))
  (send dc set-pen "black" 1 'solid) ;; default
  (for [(step (card-background (state-card (unbox now))))]
    (apply dynamic-send dc step)))

(define (find-card stack card-name)
  (hash-ref (stack-cards stack) card-name))


;;; actions

(define (button-hit? button event)
  (match (button-corners button)
    [(list left top right bottom)
     (and (<= left (send event get-x) right)
          (<= top (send event get-y) bottom))]))

(define (onclick canvas now event)
  (printf "state: ~s~n" (unbox now))
  (for [(button (card-buttons (state-card (unbox now))))]
    (when (button-hit? button event)
      (let [(action (button-action button))]
        (if (string? action)
            (to-card now (find-card (state-stack (unbox now)) action))
            (action)))
      (send canvas on-paint))))

(define (to-card now next-card)
  (swap! now update 'card (lambda (_) next-card)))


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
         [first-card (first (hash-values (stack-cards main-stack)))]
         [now (box (state first-card main-stack (hash-ref modes "normal")))]
         [frame (new frame% [label stack-name])]
         [canvas (new (card-canvas% now) [parent frame]
                      [paint-callback (curry paint now)])])
    (send frame show #t)))

;; for quick testing
;; (main "mystack.rkt")
