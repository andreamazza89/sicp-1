(define (stream-cadr s)
  (stream-car (stream-cdr s)))

(define (stream-limit stream n)
  (let ((first (stream-car stream))
        (second (stream-cadr stream)))
    (let ((abs-diff-between-first-two (abs (- first second))))
      (if (< abs-diff-between-first-two n)
        second
        (stream-limit (stream-cdr stream))))))
