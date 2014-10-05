#lang racket/gui

(require "fstruct.rkt")

(provide (struct-out card)
         (struct-out button)
         (struct-out stack)
         (struct-out state)
         (struct-out mode)
         now/c
         current-card swap! update! update-in! dict-update-in dict-ref-in
         flip index-of replace
         button-hit?)


;;; types and contracts

(fstruct button (corners action code name))

(define button/c (struct/dc button
                            ;; left top right bottom edges
                            [corners (list/c natural-number/c natural-number/c
                                             natural-number/c natural-number/c)]
                            ;; string here means jump to a given card name
                            [action (or/c string? 'code)]
                            ;; this code is only run when action is 'code
                            ;; string must evaluate to a state -> state lambda
                            [code string?]
                            ;; if the name is #f, the button is invisible
                            [name (or/c #f string?)]))

(fstruct card (name background buttons events))

(define card/c (struct/dc card
                          [name string?]
                          ;; list of method calls to dc
                          [background (listof (listof any/c))]
                          [buttons (listof button/c)]
                          ;; hash of event names to state -> state functions
                          ;; should this be generalized to properties?
                          [events (hash/c string? (or/c procedure? symbol?))]))

(fstruct stack (name cards width height))

(define stack/c (struct/dc stack
                           [name string?]
                           [cards (hash/c string? card/c)]
                           [width natural-number/c]
                           [height natural-number/c]))

(fstruct mode (name onclick onrelease onmove paint next cursor))

(fstruct state (card stack mode mouse last-mouse))

(define state/c (struct/dc state
                           ;; current card name
                           [card string?]
                           [stack stack/c]
                           [mode mode?]
                           [mouse (hash/c symbol? any/c)]
                           [last-mouse (hash/c symbol? any/c)]))

(define handler/c (or/c #f (-> state/c (is-a?/c canvas%) (is-a?/c event%)
                               state/c)))

(define mode/c (struct/dc mode
                          [name string?]
                          [onclick handler/c]
                          [onrelease handler/c]
                          [onmove handler/c]
                          [paint (or/c #f (-> state/c
                                              (is-a?/c dc<%>)
                                              (is-a?/c canvas%)
                                              any/c))]
                          [next string?]))

(define now/c (box/c state/c))


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


;;; cooper-specific utilities

(define (button-hit? event button)
  (match (button-corners button)
    [(list left top right bottom)
     (and (<= left (send event get-x) right)
          (<= top (send event get-y) bottom))]))

(define (current-card state)
  (dict-ref (stack-cards (state-stack state)) (state-card state)))

(module+ test
  (require rackunit)
  (let ([x (hash 'a (hash 'b 2 'c 3) 'd 4)])
    (check-equal? (dict-ref (dict-update x 'd + 2) 'd) 6)
    (check-equal? (dict-ref-in (dict-update-in x '(a b) + 2) '(a b)) 4)))
