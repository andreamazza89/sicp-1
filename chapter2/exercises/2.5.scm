(define (bad-log-2 x)
  (define (iter y count)
    (if (= y 2)
      count
      (iter (/ y 2) (+ count 1))))
  (iter x 1))

(define (bad-log-3 x)
  (define (iter y count)
    (if (= y 3)
      count
      (iter (/ y 3) (+ count 1))))
  (iter x 1))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (cons a b)
  (* (fast-expt 2 a) (fast-expt 3 b)))

(define (car pair)
  (if (= (remainder pair 3) 0)
    (car (/ pair 3))
    (bad-log-2 pair)))

(define (cdr pair)
  (if (= (remainder pair 2) 0)
    (cdr (/ pair 2))
    (bad-log-3 pair)))
