(define (fringe tree)
  (cond
    ((null? tree) '())
    ((list? tree) (append (fringe (car tree)) (fringe (cdr tree))))
    (#t (list tree))))

(define x (list (list 1 2) (list 3 4)))

(fringe (list 2))
(fringe x)
(fringe (list x x))
