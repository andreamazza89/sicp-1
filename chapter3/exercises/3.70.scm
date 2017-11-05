(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((<= (weight s1car) (weight s2car))
                  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                 (else
                  (cons-stream s2car
                               (merge-weighted s1 (stream-cdr s2) weight))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map
      (lambda (x) (list (stream-car s) x))
      (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define (weight-a pair)
  (+ (car pair) (cadr pair)))

(define (weight-b pair)
  (let ((i (car pair)) (j (cadr pair)))
    (+
      (* 2 i)
      (* 3 j)
      (* 5 i j))))

(define (not-divisible-by dividend . divisors)
  (none-match
    (lambda (divisor) (= (remainder dividend divisor) 0))
    divisors))

(define (none-match predicate collection)
  (if (find predicate collection)
    #f
    #t))

(define exercise-a (weighted-pairs integers integers weight-a))
(define exercise-b
  (stream-filter
    (lambda (pair)
      (and
        (not-divisible-by (car pair) 2 3 5)
        (not-divisible-by (cadr pair) 2 3 5)))
    (weighted-pairs integers integers weight-b)))

(define (show-stream-until-nth stream n)
  (define (loop current)
    (cond ((stream-null? stream) 'nothing-to-show)
          ((= current n) 'done)
          (else
            (display (stream-ref stream current))
            (newline)
            (loop (+ current 1)))))
  (loop 0))

(show-stream-until-nth exercise-a 21)
(newline)
(newline)
(show-stream-until-nth exercise-b 21)
