(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (define (circuit stream-of-current initial-voltage)
    (add-streams
      (scale-stream stream-of-current R)
      (integral
        (scale-stream stream-of-current (/ 1 C))
        initial-voltage
        dt))
  circuit)
