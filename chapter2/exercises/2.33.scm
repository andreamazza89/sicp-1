(load "little-test.rkt")

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (my-map procedure sequence)
  (accumulate
    (λ (x y) (cons (procedure x) y))
    '()
    sequence))

(define (my-append list-one list-two)
  (accumulate cons list-two list-one))

(define (my-length sequence)
  (accumulate (λ (x y) (+ y 1)) 0 sequence))

(assert-equals "map defined in terms of accumulate"
  '("ciao" "ciao")
  (my-map (λ (e) "ciao") (list 0 1)))

(assert-equals "append defined in terms of accumulate"
  '("ciao" "miao" "farro")
  (my-append (list "ciao") (list "miao" "farro")))

(assert-equals "length defined in terms of accumulate"
  3
  (my-length '(1 2 3)))
