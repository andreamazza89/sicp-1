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

(define (cubic-weight pair)
  (let ((i (car pair)) (j (cadr pair)))
    (+ (* i i i) (* j j j))))

(define cube-ordered (weighted-pairs integers integers cubic-weight))

(define (stream-walk stream)
  (cond ((stream-null? stream) '())
        ((= (cubic-weight (stream-car stream)) (cubic-weight (stream-car (stream-cdr stream))))
         (cons-stream
           (list (stream-car stream) (stream-car (stream-cdr stream)))
           (stream-walk (stream-cdr (stream-cdr stream)))))
        ('placeholderelse (stream-walk (stream-cdr stream)))))

(define ramanujans (stream-walk cube-ordered))

(define (show-stream-until-nth stream n)
  (define (loop current)
    (cond ((stream-null? stream) 'nothing-to-show)
          ((= current n) 'done)
          (else
            (display (stream-ref stream current))
            (newline)
            (loop (+ current 1)))))
  (loop 0))

(show-stream-until-nth ramanujans 12)
