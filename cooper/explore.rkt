#lang racket/gui

(require "cooper.rkt")

(provide explore-mode)

(define (create-unknown-card state button)
  (dict-update-in state '(stack cards) dict-set (button-action button)
                  (card (button-action button) '() '() (hash))))

(define (activate-button state action)
  (let* ([leave (dict-ref (card-events (current-card state)) 'leave 'identity)]
         [enter (dict-ref (card-events (current-card state)) 'enter 'identity)]
         [action-fn (λ (state) (state 'card action))])
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
                  (activate-button state action)))
            #f)) state))

(define explore-mode
  (mode "explore" "white" '() click #f #f #f "buttons"
        (make-object cursor% 'hand)))
