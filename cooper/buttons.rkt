#lang racket/gui

(require "cooper.rkt")

(provide buttons-mode render-button)

(define (make-button-corners down-event up-event)
  (list (min (send down-event get-x) (send up-event get-x))
        (min (send down-event get-y) (send up-event get-y))
        (max (send down-event get-x) (send up-event get-x))
        (max (send down-event get-y) (send up-event get-y))))

(define (existing-card? state card-name)
  (member card-name (hash-keys (stack-cards (state-stack state)))))

(define double-click-threshold 500)

(define (double-click? mouse last-mouse)
  (> double-click-threshold (- (hash-ref mouse 'at)
                               (hash-ref last-mouse 'at 0))))

(define (button-editor-frame% sem)
  (class frame%
    (super-new)
    (define (on-close)
      (semaphore-post sem))
    (augment on-close)))

(define (button-edit-window button)
  (let* ([sem (make-semaphore 0)]
         [editor (new text%)]
         ;; TODO: turn read forms back into string
         [snip (make-object string-snip% (button-action button))]
         [frame (new (button-editor-frame% sem) [label "Button Edit"]
                     [width 500] [height 500])]
         [canvas (new editor-canvas% [parent frame])]
         [mb (new menu-bar% [parent frame])]
         [m-edit (new menu% [label "Edit"] [parent mb])]
         [style (make-object style-delta% 'change-family 'modern)])
    (send editor change-style style 'start 'end)
    (append-editor-operation-menu-items m-edit #t)
    (send canvas set-editor editor)
    ;; TODO: visible/name
    ;; TODO: add ok/cancel buttons
    ;; TODO: add select menu for existing cards
    (send frame show #t)
    (send editor insert snip)
    (define (blocker) ; probably a better way to do this
      (when (not (semaphore-try-wait? sem))
        (sleep/yield 0.1)
        (blocker)))
    (blocker)
    (send editor get-flattened-text)))

(define (button-edit state target-button)
  (let* ([input (get-text-from-user "card" "card")
                ;; (button-edit-window target-button)
                ]
         [action (if #t ;(hash-ref (stack-cards (state-stack state)) input #f)
                     input
                     ;; TODO: check for readable input here
                     (read (open-input-string input)))]
         [new-button (update target-button 'action (lambda (_) action))])
    (update state 'stack update 'cards
            hash-update (state-card state) update 'buttons
            replace target-button new-button)))

(define (click state canvas event)
  (let ([target-button (findf (curry button-hit? event)
                              (card-buttons (current-card state)))])
    (if target-button
        (let ([state (update state 'mouse hash-set 'target-button target-button)])
          (if (double-click? (state-mouse state) (state-last-mouse state))
              (update (button-edit state target-button) 'mouse (lambda (_) (hash)))
              state))
        state)))

(define (release state canvas event mouse)
  (if (not (hash-ref mouse 'target-button #f))
      (let ([corners (make-button-corners (hash-ref mouse 'down)
                                          (hash-ref mouse 'last))])
        (update state 'stack
                update 'cards hash-update (state-card state)
                update 'buttons (flip cons) (button corners "" #f #f)))
      state))

(define (render-button button dc render-invisible?)
  (if (button-visible? button)
      (send dc set-pen "black" 1 'solid)
      (send dc set-pen "black" 1 'long-dash))
  (match (button-corners button)
    [(list left top right bottom)
     (when (or render-invisible? (button-visible? button))
       (send dc draw-rectangle
             (min left right) (min top bottom)
             (- (max left right) (min left right))
             (- (max top bottom) (min top bottom))))
     (when (and (button-visible? button) (button-name button))
       (send dc draw-text (button-name button) (+ 6 left) (+ 3 top)))]))

(define (paint state dc canvas)
  (send dc set-brush "white" 'transparent)
  (send dc set-smoothing 'unsmoothed)
  (for ([button (card-buttons (current-card state))])
    (render-button button dc #t))
  ;; if a button is currently being created
  (let ([down (dict-ref (state-mouse state) 'down #f)]
        [last (dict-ref (state-mouse state) 'last #f)])
    (when (and down last (not (dict-ref (state-mouse state) 'target-button #f)))
      (render-button (button (list (send down get-x) (send down get-y)
                                   (send last get-x) (send last get-y))
                             "" #f #f) dc #t))))

(define button-resize-threshold 10)

(define (button-corner-deltas corners click-x click-y dx dy)
  ;; very repetitive, room for improvement here
  (list (if (> (- (third corners) click-x) button-resize-threshold) dx 0)
        (if (> (- (fourth corners) click-y) button-resize-threshold) dy 0)
        (if (> (- click-x (first corners)) button-resize-threshold) dx 0)
        (if (> (- click-y (second corners)) button-resize-threshold) dy 0)))

(define (button-new-corners old-corners last-mouse new-mouse)
  (let* ([start-x (send last-mouse get-x)]
         [start-y (send last-mouse get-y)]
         [delta-x (- (send new-mouse get-x) start-x)]
         [delta-y (- (send new-mouse get-y) start-y)])
    (map + (button-corner-deltas old-corners start-x start-y
                                 delta-x delta-y) old-corners)))

(define (button-drag target event state)
  (let* ([card-name (state-card state)]
         [last-mouse (hash-ref (state-mouse state) 'last)]
         [new-button (update target 'corners button-new-corners last-mouse event)]
         [state (update state 'mouse hash-set 'target-button new-button)]
         [state (update state 'mouse hash-set 'target-start-xy
                        (list (send event get-x) (send event get-y)))])
    (update state 'stack update 'cards
            hash-update card-name update 'buttons
            replace target new-button)))

(define (move state canvas event)
  (if (dict-ref (state-mouse state) 'down #f)
      (let* ([target-button (dict-ref (state-mouse state) 'target-button #f)]
             [state (if target-button
                        (button-drag target-button event state)
                        state)])
        (send canvas refresh)
        state)
      state))

(define buttons-mode
  (mode "buttons" "blue" '() click release move paint "draw"))
