#lang racket/gui

(require "cooper.rkt")

(provide explore-mode)

(define (create-unknown-card state button)
  (update state 'stack update 'cards hash-set (button-action button)
          (card (button-action button) '() '() (hash))))

(define (activate-button state action)
  (let* ([leave (hash-ref (card-events (current-card state)) 'leave 'identity)]
         [enter (hash-ref (card-events (current-card state)) 'enter 'identity)])
    ((apply compose (map eval (list enter action leave))) state)))

(define (click state canvas event)
  (or (for/or [(button (card-buttons (current-card state)))]
        (if (button-hit? event button)
            (let ([action (if (string? (button-action button))
                              (lambda (state)
                                (update state 'card (lambda (_) (button-action button))))
                              (button-action button))])
              (if (and (string? (button-action button))
                       (not (hash-ref (stack-cards (state-stack state))
                                      (button-action button) #f)))
                  (if (equal? (message-box "new card?"
                                           (format "Unknown card ~s; create it?"
                                                   (button-action button))
                                           #f '[ok-cancel])'ok)
                      (create-unknown-card state button)
                      state)
                  (activate-button state action)))
            #f)) state))

(define explore-mode
  (mode "explore" "white" '() click #f #f #f "buttons"))
