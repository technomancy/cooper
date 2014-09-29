#lang racket/gui

(provide (struct-out card)
         (struct-out button)
         (struct-out stack)
         (struct-out state)
         (struct-out mode)
         current-card update swap! hash-update flip replace
         button-hit?)


;;; types

(struct card (name background buttons events) #:prefab)

(struct button (corners action name) #:prefab)

(struct stack (name cards width height) #:prefab)

(struct state (card stack mode mouse last-mouse) #:prefab)

(struct mode (name color submodes onclick onrelease onmove paint next)
        #:prefab)

(define (current-card state)
  (hash-ref (stack-cards (state-stack state)) (state-card state)))


;;; general helpers

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


;;; utilities

(define (button-hit? event button)
  (match (button-corners button)
    [(list left top right bottom)
     (and (<= left (send event get-x) right)
          (<= top (send event get-y) bottom))]))
