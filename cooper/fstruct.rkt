#lang racket

(provide fstruct)

(require (for-syntax syntax/parse
                     racket/syntax)
         racket/match
         racket/serialize)

;; A functional struct macro.

;; This adds a few features over normal transparent structs including
;; supporting dictionary methods and being applicable. Calling an
;; fstruct with a single symbol argument reads from that field, while
;; calling with a field and value will return a new instance with that
;; field set to that value.

;; (fstruct abc (a b c))
;;
;; ((abc 1 2 3) 'b) ; -> 2
;; ((abc 1 2 3) 'c 8) ; -> (abc 1 2 8)
;; (dict-ref (abc 1 2 3) 'a) ; -> 1
;; (dict-set (abc 1 2 3) 'b 4) ; -> (abc 1 4 3)
;; (dict-update (abc 1 2 3) 'c (curry + 10)) ; -> (abc 1 2 13)

(define-syntax (fstruct stx)
  (syntax-parse stx
    [(_ id:id (field:id ...))
     (with-syntax ([(accessor ...)
                    (for/list ([fld (in-list (syntax->list #'(field ...)))])
                      (format-id stx "~a-~a" (syntax->datum #'id) fld))]
                   [field-next (let* ([fs (syntax->datum #'(field ...))]
                                      [h (for/hash ([(f i) (in-indexed fs)])
                                           (values f (if (< (length fs) i)
                                                         (list-ref fs (add1 i))
                                                         #f)))])
                                 (datum->syntax stx h))])

       ;; Thanks to Greg Hendershott for making this a proper hygenic macro
       #'(serializable-struct
          id (field ...) #:transparent
          #:property prop:procedure
          (lambda (self . args)
            (match args
              [(list 'field) (accessor self)] ...
              [(list (list 'field)) (accessor self)] ...
              [(list (list-rest 'field fields)) ((accessor self) fields)] ...
              [(list 'field val)
               (struct-copy id self [field val])] ...
              [(list (list 'field) val) (self 'field val)] ...
              [(list (list-rest 'field fields) val)
               (struct-copy id self
                            [field ((accessor self) fields val)])] ...))

          #:methods gen:dict
          [(define (dict-ref dict key
                             [default (λ () (error "key not found" key))])
             (if (hash-has-key? field-next key)
                 (dict key)
                 (default key)))
           (define (dict-set dict key val)
             (dict key val))
           (define (dict-iterate-first dict)
             (and (not (empty? field-next))
                  (first (hash-keys field-next))))
           (define (dict-iterate-next dict pos)
             (if (hash-has-key? field-next pos)
                 (hash-ref field-next pos)
                 (error "key not found" pos)))
           (define (dict-iterate-key dict pos)
             (and (hash-has-key? field-next pos) pos))
           (define (dict-iterate-value dict pos)
             (and (hash-has-key? field-next pos) (dict pos)))]))]))

(module+ test
  (require rackunit)
  (fstruct abc (a b c))

  (let ([s (abc 1 2 3)])
    ;; Single argument for access
    (check-equal? (s 'a) 1)
    (check-equal? (s 'b) 2)
    (check-equal? (s 'c) 3)

    ;; 2 args for setting a field
    (check-equal? (abc 2 2 3) (s 'a 2)))

  ;; nested access
  (let ([nested (abc 1 (abc 1 2 (abc 1 2 3)) 3)])
    (check-equal? (nested '(c)) 3)
    (check-equal? (nested '(b b)) 2)
    (check-equal? (nested '(b c a)) 1)

    ;; nested updates
    (let ([nested2 (nested '(c) 4)]
          [nested3 (nested '(b b) 5)]
          [nested4 (nested '(b c a) 9)])
      (check-equal? (nested2 'c) 4)
      (check-equal? (nested3 '(b b)) 5)
      (check-equal? (nested4 '(b c a)) 9)))

  ;; dict operations
  (let* ([nested (abc 1 (abc 1 2 (abc 1 2 3)) 3)]
         [nested2 (dict-set nested 'a 2)]
         [nested3 (dict-update nested2 'a (curryr + 2))]
         [nested4 (dict-update nested 'b (curryr dict-update
                                                 'a (curryr + 2)))])
    (print (equal? nested nested4))
    (check-equal? (dict-ref nested 'a) 1)
    (check-equal? (dict-ref nested2 'a) 2)
    (check-equal? (dict-ref nested3 'a) 4)
    (check-equal? (dict-ref (dict-ref nested4 'b) 'a) 3)))
