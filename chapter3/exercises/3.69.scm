(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
              (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t))))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map
        (lambda (x) (cons (stream-car s) x))
        (pairs (stream-cdr t) (stream-cdr u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define pythagoreas (stream-filter
  (lambda (triplet)
    (and
      (= (square (caddr triplet)) (+ (square (car triplet)) (square (cadr triplet))))
      (<= (car triplet) (cadr triplet))))
  (triples integers integers integers)))

(display (stream-ref pythagoreas 3))
