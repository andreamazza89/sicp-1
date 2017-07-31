(load "little-test.rkt")

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap op sequence)
  (accumulate append '() (map op sequence)))

(define (ordered-triples range-end sum)
  (filter
    (lambda (triple) (= sum (accumulate + 0 triple)))
    (flatmap
      (lambda (i)
        (flatmap
          (lambda (j)
            (map
              (lambda (k) (list k j i))
              (range 1 j)))
          (range 1 i)))
      (range 1 (+ range-end 1)))))

(assert-equals "finds all ordered triples that add up to seven"
  (list (list 1 3 4) (list 1 2 5))
  (ordered-triples 5 8))
