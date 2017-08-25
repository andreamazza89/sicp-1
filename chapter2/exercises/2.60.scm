(load "little-test.rkt")

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(assert-equals "nothing could belong to an empty set"
  #f
  (element-of-set? 4 '()))

(assert-equals "knows if something is not in a set"
  #f
  (element-of-set? 1 '(2 3 4)))

(assert-equals "knows if somethign is in a set"
  #t
  (element-of-set? 5 '(5 6 7 5)))

(assert-equals "adjoins an element to an empty set"
  '(4)
  (adjoin-set 4 '()))

(assert-equals "adjoins an element to a set"
  '(4 5 7 7 8)
  (adjoin-set 4 '(5 7 7 8)))

(assert-equals "adjoins an element to a set even when it already exists"
  '(3 3 3 4 4 5 5)
  (adjoin-set 3 '(3 3 4 4 5 5)))

(assert-equals "uniting two empty sets results in an empty set"
  '()
  (union-set '() '()))

(assert-equals "union results in all elements from both sets"
  '(1 1 2 2 3 3 4 4 5 5)
  (union-set '(1 1 2 2 3 3) '(4 4 5 5)))

(assert-equals "intersection between empty sets is an empty set"
  '()
  (intersection-set '() '()))

(assert-equals "intersection between an empty set and a non-empty one is the non-empty one"
  '()
  (intersection-set '() '(1 2 3)))

(assert-equals "intersection between two sets is a set of the common elements"
  '(1 2 3)
  (intersection-set '(1 2 3 4 4 5) '(1 2 3 8 9)))
