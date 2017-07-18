(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (full-fermat n a)
  (cond ((= a n) true)
        ((= (expmod a n n) a)
          (full-fermat n (+ a 1)))
        (else false)))

(full-fermat 561 1)
(full-fermat 1105 1)
(full-fermat 1729 1)
(full-fermat 2465 1)
(full-fermat 2821 1)
(full-fermat 6601 1)
; Don't get fooled by the rocks that I got, I'm still I'm still Fermat from the block
