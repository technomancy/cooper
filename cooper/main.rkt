#lang racket/gui

(require "cooper.rkt" "explore.rkt" "buttons.rkt" "draw.rkt")


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
      [(#\0)
       (swap! now update 'card (lambda (_) "zero"))
       (swap! now zero-enter)])
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

(define motion-drop-threshold 100)
(define last-motion (box 0))

;; kind of a terrible hack
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
                           (lambda () #f) now this event))
    (define/override (on-event event)
      (when (or (not (equal? 'motion (send event get-event-type)))
                (not (drop-motion? event)))
        (call-with-semaphore semaphore handle-mouse
                             (lambda () #f) now this event)))
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


;;; card zero

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

(define (zero-copy-card card state)
  (let* ([new-card (update card 'name string-append " copy")])
    (zero-enter (update state 'stack update 'cards hash-set
                        (card-name new-card) new-card))))

(define (zero-place-delete-button card i buttons)
  (cons (button (list (+ 215 zero-button-x-offset)
                      (* zero-button-y-offset (add1 i))
                      (+ 240 zero-button-x-offset)
                      (- (* zero-button-y-offset (+ 2 i)) 5))
                `(lambda (state) (zero-delete-card ,card state)) #t "x")
        buttons))

(define (zero-place-copy-button card i buttons)
  (cons (button (list (+ 260 zero-button-x-offset)
                      (* zero-button-y-offset (add1 i))
                      (+ 285 zero-button-x-offset)
                      (- (* zero-button-y-offset (+ 2 i)) 5))
                `(lambda (state) (zero-copy-card ,card state)) #t "+")
        buttons))

(define (zero-buttons stack card)
  (let* ([buttons (foldl zero-place-button '()
                             (hash-values (stack-cards stack))
                             (range (hash-count (stack-cards stack))))]
        [buttons (foldl zero-place-delete-button buttons
                               (hash-values (stack-cards stack))
                               (range (hash-count (stack-cards stack))))]
        [buttons (foldl zero-place-copy-button buttons
                             (hash-values (stack-cards stack))
                             (range (hash-count (stack-cards stack))))])
    (update card 'buttons (lambda (_) (cons zero-new-card-button buttons)))))

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

(define modes `#hash(("explore" . ,explore-mode)
                     ("buttons" . ,buttons-mode)
                     ("draw" . ,draw-mode)))

(module+ main
  (main))
;; for quick testing
;; (main "mystack.rkt")
