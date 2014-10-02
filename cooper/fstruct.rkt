#lang racket

(provide fstruct)

(require (for-syntax syntax/parse
                     racket/syntax)
         racket/match
         racket/serialize)

(define-syntax (fstruct stx)
  (syntax-parse
   stx [(_ id:id (field:id ...))
        (with-syntax ([(accessor ...)
                       (for/list ([fld (in-list (syntax->list #'(field ...)))])
                         (format-id stx "~a-~a" (syntax->datum #'id) fld))])
          #'(serializable-struct
             id (field ...) #:transparent
             #:property prop:procedure
             (lambda (self . args)
               (match args
                 [(list 'field)
                  (accessor self)] ...
                 ;; TODO: support nested fstructs with field-path lists
                 [(list-rest 'field f args)
                   (struct-copy id self
                                [field (apply f (accessor self) args)])]
                  ...))))]))

(module+ test
  (require rackunit)
  (fstruct abc (a b c))

  (let ([s (abc 1 2 3)])
    ;; Single argument for access
    (check-equal? (s 'a) 1)
    (check-equal? (s 'b) 2)
    (check-equal? (s 'c) 3)

    ;; 2+ args for updating a field
    (let ([s2 (s 'a + 1)])
      (check-equal? (s2 'a) 2)
      (check-equal? (s2 'b) 2)
      (check-equal? (s2 'c) 3))))
