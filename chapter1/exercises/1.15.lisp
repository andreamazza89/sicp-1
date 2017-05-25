(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

; for (sine 12.15)
; 1 (p (sine 4.05))
; 2 (p (sine 1.35))
; 3 (p (sine 0.45))
; 4 (p (sine 0.15))
; 5 (p 0.05)

; 5 times


; both O(log3(a)) for (sine a)
