#lang racket/gui

(require "cooper.rkt")

(provide explore-mode)

(define (click state canvas event)
  (or (for/or [(button (card-buttons (current-card state)))]
        (if (button-hit? event button)
            (if (or (hash-ref (stack-cards (state-stack state))
                              (button-action button) #f)
                    (procedure? (eval (button-action button))))
                (let* ([action (button-action button)]
                       [leave (hash-ref (card-events (current-card state))
                                        'leave 'identity)]
                       [action (if (string? action)
                                   (lambda (state)
                                     (update state 'card (lambda (_) action)))
                                   action)]
                       [enter (hash-ref (card-events (current-card state))
                                        'enter 'identity)])
                  ((apply compose (map eval (list enter action leave))) state))
                (if (equal? (message-box "new card?"
                                         (format "Unknown card ~s; create it?"
                                                 (button-action button))
                                         #f '[ok-cancel])'ok)
                    (update state 'stack
                            update 'cards hash-set (button-action button)
                            (card (button-action button)
                                  '() '() (hash)))
                    state))
            #f)) state))

(define explore-mode
  (mode "explore" "white" '() click #f #f #f "buttons"))
