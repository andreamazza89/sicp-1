(define counted-ones '())
(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       (if (already-counted? x counted-ones)
         0
         (begin
           (set! counted-ones (cons x counted-ones))
           1)))))


(define (already-counted? element counted-ones)
  (member element counted-ones))

(count-pairs (cons 'a (cons 'b (cons 'c '()))))

(define p1 (cons 'a 'b))
(define p2 (cons 'c 'd))
(define p3 (cons 'e '()))
;;;;;;;;;;;;;;;;;;;;;;;;
(set-car! p1 p2)
(set-cdr! p1 p3)
(set-cdr! p2 p3)
(display "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv")
(count-pairs p1)



(define p12 (cons 'a 'b))
(define p22 (cons 'c 'd))
(define p32 (cons 'e '()))
;;;;;;;;;;;;;;;;;;;;;;;;;
(set-car! p12 p22)
(set-cdr! p12 p22)
(set-car! p22 p32)
(set-cdr! p22 p32)
(display "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv")
p12
(count-pairs p12)
