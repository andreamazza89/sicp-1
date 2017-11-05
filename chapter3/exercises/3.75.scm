(define (sign-change-detector before now)
  (cond
    ((and (< now 0) (> before 0)) -1)
    ((and (> now 0) (< before 0)) 1)
    (else 0)))

(define (average a b)
  (/ (+ a b) 2))

(define (make-zero-crossings input-stream last-value last-average)
  (let ((avpt (average (stream-car input-stream) last-value)))
    (cons-stream (sign-change-detector avpt last-average)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))
