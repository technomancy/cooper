#lang racket/gui

(require "cooper.rkt" "explore.rkt" "buttons.rkt" "draw.rkt"
         racket/serialize)

(define modes `#hash(("explore" . ,explore-mode)
                     ("buttons" . ,buttons-mode)
                     ("draw" . ,draw-mode)))


;;; imperative bits

(define debug (box #f))

(define (next-mode mode)
  (dict-ref modes (mode-next mode)))

(define (handle-key now canvas event)
  (when (send event get-control-down)
    (case (send event get-key-code)
      ;; save
      [(#\s) (let ([filename (put-file "Save to:")])
               (when filename
                 (when (file-exists? filename)
                   (delete-file filename))
                 (call-with-output-file filename
                   (λ (port)
                     (write (serialize (state-stack (unbox now))) port)))))]
      ;; load
      [(#\l) (let ([filename (get-file "Load:")])
               (when filename
                 (update! now 'stack
                          (λ _ (call-with-input-file filename
                                 (compose zero-enter deserialize read))))
                 ;; TODO: not all stacks have a zero card?
                 (update! now 'card (lambda _ "zero"))))]
      ;; new card
      [(#\n) (swap! now zero-new-card)]
      ;; clear background
      [(#\c) (update-in! now `(stack cards ,(state-card (unbox now)) background)
                      (λ _ '()))]
      ;; clear buttons
      [(#\b) (update-in! now `(stack cards ,(state-card (unbox now)) buttons)
                      (λ _ '()))]
      ;; navigate to card zero
      [(#\0)
       (update! now 'card (lambda _ "zero"))
       (swap! now zero-enter)])
    (send canvas refresh)))

(define (handle-mouse now canvas event)
  (case (send event get-event-type)
    [(left-down)
     (update-in! now '(mouse) dict-set 'down event)
     (update-in! now '(mouse) dict-set 'last event)
     (update-in! now '(mouse) dict-set 'at (current-milliseconds))
     (let ([onclick (mode-onclick (state-mode (unbox now)))])
       (and onclick (swap! now onclick canvas event))
       (send canvas refresh))]
    [(left-up)
     (let ([mouse (state-mouse (unbox now))]
           [onrelease (mode-onrelease (state-mode (unbox now)))])
       (update! now 'mouse (λ _ (hash)))
       (update! now 'last-mouse (λ _ mouse))
       (and onrelease (swap! now onrelease canvas event mouse))
       (send canvas refresh))]
    [(motion)
     (let ([onmove (mode-onmove (state-mode (unbox now)))])
       (and onmove (swap! now onmove canvas event))
       (update-in! now '(mouse) dict-set 'last event))]
    [(right-down) ; TODO: change modes via right-click menu
     (update-in! now '(mode) next-mode)
     (update-in! now '(mouse) (λ _ (hash)))
     (send canvas set-cursor (dict-ref-in (unbox now) '(mode cursor)))
     (send canvas refresh)]))

(define motion-drop-threshold 100)
(define last-motion (box 0))

;; without this we get spammed with mouse motion events much faster than we can
;; deal with them. kind of a terrible hack.
(define (drop-motion? event)
  (if (< (- (send event get-time-stamp)
            (unbox last-motion)) motion-drop-threshold)
      #t
      (begin (set-box! last-motion (send event get-time-stamp))
             #f)))

(define (card-canvas% now semaphore)
  (class canvas%
    (when (unbox debug)
      (printf "state: ~s~n" (unbox now)))
    (define/override (on-char event)
      (call-with-semaphore semaphore handle-key
                           (lambda () #f) now this event)
      (send (send this get-parent) set-label
            (string-append "Cooper: " (state-card (unbox now)))))
    (define/override (on-event event)
      (when (or (not (equal? 'motion (send event get-event-type)))
                (not (drop-motion? event)))
        (call-with-semaphore semaphore handle-mouse
                             (lambda () #f) now this event)
        (send (send this get-parent) set-label
              (string-append "Cooper: " (state-card (unbox now))))))
    (super-new)))

;; we re-draw from scratch every time! lots of room for optimization here.
(define (paint now canvas dc)
  (let ([state (unbox now)])
    (send dc clear)
    (send dc set-brush "white" 'solid)
    (send dc set-pen "black" 1 'solid) ; default
    (send dc set-smoothing 'unsmoothed)
    (for [(step (card-background (current-card state)))]
      (apply dynamic-send dc step))
    (for [(button (card-buttons (current-card state)))]
      (render-button button dc #f))
    (let ([painter (mode-paint (state-mode state))])
      (when painter
        (painter (unbox now) dc canvas)))))


;;; card zero is the index; contains buttons to all the other cards

(define zero-button-x-offset 25)

(define zero-button-y-offset 30)

(define (zero-place-button card i buttons)
  (cons (button (list zero-button-x-offset (* zero-button-y-offset (add1 i))
                      200 (- (* zero-button-y-offset (+ 2 i)) 5))
                (card-name card) "" (card-name card))
        buttons))

(define (zero-delete-card card state)
  (zero-enter (dict-update-in state '(stack cards)
                              dict-remove (card-name card))))

(define (zero-copy-card card state)
  (let* ([new-card (card 'name string-append " copy")])
    (zero-enter (dict-update-in state '(stack cards)
                                dict-set (card-name new-card) new-card))))

(define (zero-place-delete-button card i buttons)
  (cons (button (list (+ 215 zero-button-x-offset)
                      (* zero-button-y-offset (add1 i))
                      (+ 240 zero-button-x-offset)
                      (- (* zero-button-y-offset (+ 2 i)) 5))
                'code (~s "(lambda (state) (zero-delete-card ~s state))" card) "x")
        buttons))

(define (zero-place-copy-button card i buttons)
  (cons (button (list (+ 260 zero-button-x-offset)
                      (* zero-button-y-offset (add1 i))
                      (+ 285 zero-button-x-offset)
                      (- (* zero-button-y-offset (+ 2 i)) 5))
                'code (~s "(lambda (state) (zero-copy-card ~s state))" card) "x")
        buttons))

(define (zero-buttons stack card)
  (let* ([buttons (foldl zero-place-button '()
                         (dict-values (stack-cards stack))
                         (range (dict-count (stack-cards stack))))]
         [buttons (foldl zero-place-delete-button buttons
                         (dict-values (stack-cards stack))
                         (range (dict-count (stack-cards stack))))]
         [buttons (foldl zero-place-copy-button buttons
                         (dict-values (stack-cards stack))
                         (range (dict-count (stack-cards stack))))])
    (card 'buttons (cons zero-new-card-button buttons))))

;; this function refreshes card zero's button list
(define (zero-enter state)
  (dict-update-in state `(stack cards "zero")
          (curry zero-buttons (state-stack state))))

(define (zero-new-card state)
  (let ([card-name (get-text-from-user "card" "New card name:")])
    (if card-name
        (zero-enter (dict-update-in state '(stack cards)
                                    dict-set card-name
                                    (card card-name '() '() (hash))))
        state)))

(define zero-new-card-button (button '(0 0 25 30) 'code "zero-new-card" "+"))

(define card-zero (card "zero" '() '() (hash "enter" 'zero-enter)))

(define card-one (card "one" '() '() (hash)))


;;; loading

(module+ main
  (let* ([stack (stack "new" (hash "zero" card-zero "one" card-one) 800 600)]
         [now (box (zero-enter (state "zero" stack (dict-ref modes "explore")
                                      (hash) (hash))))]
         [now (if (unbox debug)
                  (contract now/c now 'valid-state 'invalid-state)
                  now)]
         [frame (new frame% [label "Cooper"]
                     [width (stack-width stack)] [height (stack-height stack)])]
         [canvas (new (card-canvas% now (make-semaphore 1)) [parent frame]
                      [paint-callback (curry paint now)])])
    (send frame show #t)
    (send canvas set-cursor (mode-cursor explore-mode))
    now))
