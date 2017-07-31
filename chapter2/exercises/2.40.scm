(load "little-test.rkt")

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap op sequence)
  (accumulate append '() (map op sequence)))

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
           (range 1 i)))
    (range 1 (+ n 1))))

(assert-equals "finds the unique pairs"
  (list (list 2 1) (list 3 1) (list 3 2))
  (unique-pairs 3))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
