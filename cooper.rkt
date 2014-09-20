#lang racket/gui


;; types

(struct card (name background buttons) #:prefab)

(struct button (corners action) #:prefab)

(struct stack (name cards) #:prefab)

(struct state (card stack edit?) #:prefab)


;; cards

(define (button-hit? button event)
  (match (button-corners button)
    [(list left top right bottom)
     (and (<= left (send event get-x) right)
          (<= top (send event get-y) bottom))]))

(define/match (toggle-edit _state)
  [((state card stack edit?)) (state card stack (not edit?))])

(define (card-canvas% now)
  (class canvas%
    (define/override (on-event event)
      (when (eq? 'left-down (send event get-event-type))
        (on-click this now event))
      (when (eq? 'right-down (send event get-event-type))
        (set-box! now (toggle-edit (unbox now)))
        (send this on-paint)))
    (super-new)))

(define (paint now canvas dc)
  (send dc clear)
  (send dc set-brush "white" 'solid)
  (when (state-edit? (unbox now))
    (send dc set-pen "blue" 2 'solid)
    (let-values ([(width height) (send canvas get-client-size)])
      (send dc draw-rectangle 0 0 width height)))
  (send dc set-pen "black" 1 'solid) ;; default
  (for [(step (card-background (state-card (unbox now))))]
    (apply dynamic-send dc step)))

(define (find-card stack card-name)
  (hash-ref (stack-cards stack) card-name))


;; actions

(define (on-click canvas now event)
  (for [(button (card-buttons (state-card (unbox now))))]
    (when (button-hit? button event)
      (let [(action (button-action button))]
        (if (string? action)
            (to-card now (find-card (state-stack (unbox now)) action))
            (action)))
      (send canvas on-paint))))

(define (to-card now next-card)
  ;; swap!/update-in would sure be nice here
  (set-box! now (state next-card (state-stack (unbox now))
                       (state-edit? (unbox now)))))


;; loading

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
         [now (box (state first-card main-stack #f))]
         [frame (new frame% [label stack-name])]
         [canvas (new (card-canvas% now) [parent frame]
                      [paint-callback (curry paint now)])])
    (send frame show #t)))

;; for quick testing
;; (main "mystack.rkt")

