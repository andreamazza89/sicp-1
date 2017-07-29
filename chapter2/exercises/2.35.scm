(load "little-test.rkt")

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (fringe tree)
  (cond
    ((null? tree) '())
    ((list? tree) (append (fringe (car tree)) (fringe (cdr tree))))
    (#t (list tree))))

(define (count-leaves tree)
  (accumulate
    (lambda (leaves total) (+ (length leaves) total))
    0
    (map fringe tree)))

;;;;;;;
;; test
;;;;;;;

(assert-equals "an empty tree has no leaves"
  0
  (count-leaves '()))

(assert-equals "counts a one-node tree"
  1
  (count-leaves (list 42)))

(assert-equals "counts a more complex tree"
  4
  (count-leaves (list (list 2 (list 4 5)) (list 1))))
