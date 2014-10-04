#lang racket/gui

(require "cooper.rkt")

(provide draw-mode)

(define (release state canvas event mouse)
  (let ([down (dict-ref mouse 'down)])
    (dict-update-in state `(stack cards ,(state-card state) background)
                    (flip cons) (list 'draw-line
                                      (send down get-x)
                                      (send down get-y)
                                      (send event get-x)
                                      (send event get-y)))))

(define (paint state dc canvas)
  (send dc set-pen "black" 1 'solid)
  (let ([down (dict-ref (state-mouse state) 'down #f)]
        [last (dict-ref (state-mouse state) 'last #f)])
    (when (and down last)
      (send dc draw-line
            (send down get-x) (send down get-y)
            (send last get-x) (send last get-y)))))

(define (move state canvas event)
  (when (dict-ref (state-mouse state) 'down #f)
    (send canvas refresh))
  state)

(define draw-mode
  (mode "draw" "red" '() #f release move paint "explore"))
