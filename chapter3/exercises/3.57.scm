(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

; for every element computed by fibs, we also have to compute (stream-cdr fibs), unless
; delay is optimised.
