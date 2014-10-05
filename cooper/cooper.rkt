#lang racket/gui

(require "fstruct.rkt")

(provide (struct-out card)
         (struct-out button)
         (struct-out stack)
         (struct-out state)
         (struct-out mode)
         current-card swap! update! update-in! dict-update-in dict-ref-in
         flip index-of replace
         button-hit?)


;;; types

(fstruct card (name background buttons events))

(fstruct button (corners action code name))

(fstruct stack (name cards width height))

(fstruct state (card stack mode mouse last-mouse))

(fstruct mode (name color submodes onclick onrelease onmove paint next cursor))

(define (current-card state)
  (dict-ref (stack-cards (state-stack state)) (state-card state)))


;;; general helpers

(define (swap! box f . args)
  (set-box! box (apply f (unbox box) args)))

(define (update! box keys f)
  (set-box! box (dict-update (unbox box) keys f)))

(define (update-in! box keys f . args)
  (set-box! box (apply dict-update-in (unbox box) keys f args)))

(define (dict-update-in d ks f . args)
  (if (empty? (rest ks))
      (dict-update d (first ks) (λ (x) (apply f x args)))
      (dict-set d (first ks) (apply dict-update-in (dict-ref d (first ks))
                                    (rest ks) f args))))

(define (dict-ref-in d ks)
  (if (empty? (rest ks))
      (dict-ref d (first ks))
      (dict-ref-in (dict-ref d (first ks)) (rest ks))))

(define (flip f)
  (lambda args (apply f (reverse args))))

(define (replace lst old new)
  (map (lambda (x) (if (equal? x old) new x)) lst))

(define (index-of l x) ; how is this missing?
  (for/or ([y l] [i (in-naturals)] #:when (equal? x y)) i))


;;; utilities

(define (button-hit? event button)
  (match (button-corners button)
    [(list left top right bottom)
     (and (<= left (send event get-x) right)
          (<= top (send event get-y) bottom))]))

(module+ test
  (require rackunit)
  (let ([x (hash 'a (hash 'b 2 'c 3) 'd 4)])
    (check-equal? (dict-ref (dict-update x 'd + 2) 'd) 6)
    (check-equal? (dict-ref-in (dict-update-in x '(a b) + 2) '(a b)) 4)))
