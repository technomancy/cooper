#lang racket

(provide fstruct)

(require (for-syntax syntax/parse
                     racket/syntax)
         racket/match
         racket/serialize)

(define-syntax (fstruct stx)
  (syntax-parse stx
    [(_ id:id (field:id ...))
     (with-syntax ([(accessor ...)
                    (for/list ([fld (in-list (syntax->list #'(field ...)))])
                      (format-id stx "~a-~a" (syntax->datum #'id) fld))])
       ;; Thanks to Greg Hendershott for making this a proper hygenic macro
       #'(serializable-struct
          id (field ...) #:transparent
          #:property prop:procedure
          (lambda (self . args)
            (match args
              [(list 'field) (accessor self)] ...
              [(list (list 'field)) (accessor self)] ...
              [(list (list-rest 'field fields)) ((accessor self) fields)] ...
              [(list-rest 'field f args)
               (struct-copy id self [field (apply f (accessor self) args)])] ...
              [(list-rest (list 'field) f args)
               (struct-copy id self [field (apply f (accessor self) args)])] ...
              [(list-rest (list-rest 'field fields) args)
               (struct-copy id self
                            [field (apply (accessor self) fields args)])] ...))))]))

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
      (check-equal? (s2 'c) 3)))

  ;; nested access
  (let ([nested (abc 1 (abc 1 2 (abc 1 2 3)) 3)])
    (check-equal? (nested '(c)) 3)
    (check-equal? (nested '(b b)) 2)
    (check-equal? (nested '(b c a)) 1)

    ;; nested updates
    (let ([nested2 (nested '(c) + 1)]
          [nested3 (nested '(b b) + 3)]
          [nested4 (nested '(b c a) + 9)])
      (check-equal? (nested2 'c) 4)
      (check-equal? (nested3 '(b b)) 5)
      (check-equal? (nested4 '(b c a)) 10))))
