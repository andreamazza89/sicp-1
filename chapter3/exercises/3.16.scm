(define (count-pairs x) (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs (cons 'a (cons 'b (cons 'c '()))))  ; just 3 pairs, simple list
; 'a|-> 'b|-> 'c|'()

(define p1 (cons 'a 'b))
(define p2 (cons 'c 'd))
(define p3 (cons 'e '()))
;;;;;;;;;;;;;;;;;;;;;;;;
(set-car! p1 p2)
(set-cdr! p1 p3)
(set-cdr! p2 p3)
(count-pairs p1) ; 4 pairs


(define p11 (cons 'a 'b))
(define p21 (cons 'c 'd))
(define p31 (cons 'e '()))
;;;;;;;;;;;;;;;;;;;;;;;;;
(set-car! p11 p21)
(set-cdr! p11 p31)
(set-car! p21 p31)
(set-cdr! p21 p11)
;(count-pairs p11) ; never returns

(define p12 (cons 'a 'b))
(define p22 (cons 'c 'd))
(define p32 (cons 'e '()))
;;;;;;;;;;;;;;;;;;;;;;;;;
(set-car! p12 p22)
(set-cdr! p12 p22)
(set-car! p22 p32)
(set-cdr! p22 p32)
(count-pairs p12) ; 7 pairs
