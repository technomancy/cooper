#lang racket/gui

(require "fstruct.rkt")

(provide (struct-out card)
         (struct-out button)
         (struct-out stack)
         (struct-out state)
         (struct-out mode)
         current-card swap! update! update hash-update flip replace
         button-hit?)


;;; types

(fstruct card (name background buttons events))

(fstruct button (corners action name))

(fstruct stack (name cards width height))

(fstruct state (card stack mode mouse last-mouse))

(fstruct mode (name color submodes onclick onrelease onmove paint next))

(define (current-card state)
  (hash-ref (stack-cards (state-stack state)) (state-card state)))


;;; general helpers

(define (swap! box f . args)
  (set-box! box (apply f (unbox box) args)))

;; For boxed fstructs
(define (update! box . args)
  (set-box! box (apply (unbox box) args)))

;; TODO: could we get rid of this if fstruct supported dict-update?
(define (update f . args)
  (apply f args))

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
