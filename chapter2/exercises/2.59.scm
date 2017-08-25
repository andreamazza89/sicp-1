(load "little-test.rkt")

(define (make-set elements)
  elements) ;currently just a list, so it has to be one of unique elements

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond
    ((null? set1) set2)
    ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
    (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))


(assert-equals "generates a union set"
  (make-set '(c b a d e f))
  (union-set (make-set '(a b c d)) (make-set '(d e f))))

(assert-equals "generates another union set"
  (make-set '(x y z))
  (union-set (make-set '(x y)) (make-set '(y z))))
