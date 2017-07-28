(load "little-test.rkt")

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest
          (map (λ (subset) (cons (car s) subset)) rest)))))

;;;;;;;;
;; tests
;;;;;;;;

(assert-equals "subsets of an empty set are an empty set"
  (list '())
	(subsets '()))

(assert-equals "a one-item set only has two subsets"
  (list '() (list 2))
	(subsets (list 2)))

(assert-equals "extracts all the subsets in a set"
  (list '() (list 3) (list 2) (list 2 3) (list 1) (list 1 3) (list 1 2) (list 1 2 3))
  (subsets (list 1 2 3)))
