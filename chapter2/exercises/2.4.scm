(define (cons x y)
    (lambda (m) (m x y)))
(define (car z)
    (z (lambda (p q) p)))
(define (cdr z)
    (z (lambda (p q) q)))

;;proof for car

;;(car (cons 5 6))
;;(car (lambda (m) (m 5 6)))
;;((lambda (m) (m 5 6)) (lambda (p q) p))
;;5
;;
;;
;;(cdr (cons 5 6))
;;(cdr (lambda (m) (m 5 6)))
;;((lambda (m) (m 5 6)) (lambda (p q) q))
;;6

(car (cons 55 3))
(cdr (cons 43 23))
