#lang racket/gui

(require "cooper.rkt")

(provide explore-mode)

(define (navigate-to state card-name)
  (let ([leave (dict-ref (card-events (current-card state)) 'leave 'identity)]
        [enter (dict-ref (card-events (current-card state)) 'enter 'identity)]
        [action-fn (λ (state) (state 'card card-name))])
    ((apply compose (map eval (list enter action-fn leave))) state)))

(define (run-code code state)
  (with-handlers ([exn:fail? (lambda (exn)
                               (message-box "Problem with code"
                                            (exn-message exn)
                                            false '(ok stop))
                               false)])
    (let ([code (eval (read (open-input-string code)))])
      (if (procedure? code)
          (code state)
          (error "Button code is not a procedure:" code)))))

(define (click state canvas event)
  (or (for/or [(button (card-buttons (current-card state)))]
        (if (button-hit? event button)
            (let ([action (button-action button)])
              (if (eq? 'code action)
                  (run-code (button-code button) state)
                  (navigate-to state action)))
            #f)) state))

(define explore-mode
  (mode "explore" click #f #f #f "buttons"
        (make-object cursor% 'hand)))
