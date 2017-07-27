(load "little-test.rkt")

(define (inc x) (+ x 1))

(define (map-tree procedure tree)
  (cond ((null? tree) '())
        ((list? tree) (map (lambda (node) (map-tree procedure node)) tree))
        (#t (procedure tree))))

(assert-equals "mapping over an empty tree"
  '()
  (map-tree inc '()))

(assert-equals "mapping over the leaf of a tree"
  42
  (map-tree inc 41))

(assert-equals "mapping over a simple tree"
  (list 42 2 5)
  (map-tree inc (list 41 1 4)))

(assert-equals "mapping over a tree"
  (list 42 (list 2 5))
  (map-tree inc (list 41 (list 1 4))))
