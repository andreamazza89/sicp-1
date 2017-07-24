(define (same-parity . numbers)
  (if (odd? (car numbers))
    (filter odd? numbers)
    (filter even? numbers)))

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)
