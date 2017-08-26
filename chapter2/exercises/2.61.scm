(load "little-test.rkt")

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (adjoin-set element set)
  (cond
    ((element-of-set? element set) set)
    ((null? set) (list element))
    ((< element (car set)) (cons element set))
    (else (cons (car set) (adjoin-set element (cdr set))))))

(assert-equals "adjoins an element to an empty set"
  '(42)
  (adjoin-set 42 '()))

(assert-equals "adjoins an element to the beginning of a set"
  '(1 2)
  (adjoin-set 1 '(2)))

(assert-equals "adjoins an element to a set"
  '(1 2 3 4 5)
  (adjoin-set 3 '(1 2 4 5)))

(assert-equals "adjoining an existing element is the original set"
  '(1 2 3 4)
  (adjoin-set 3 '(1 2 3 4)))
