(define zero
  (lambda (f)
    (lambda (x) x)))

(define one
  (lambda (f)
    (lambda (x) (f x))))

(define two
  (lambda (f)
      (lambda (x) (f (f x)))))

(define (add-1 n)
    (lambda (f)
      (lambda (x)
        (f ((n f) x)))))

;using substitution to try figure out (add-1 zero)

;(add-1 zero)
;
;(add-1 (lambda (f0)
;         (lambda (x0) x0)))
;
;(lambda (f)
;      (lambda (x)
;        (f (((lambda (f0) (lambda (x0) x0)) f) x))))
;
;(lambda (f)
;      (lambda (x) (f x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;using substitution to try figure out (add-1 one)

;(add-1 one)
;
;(add-1 (lambda (f)
;    (lambda (x) (f x))))
;
;(lambda (f)
;      (lambda (x)
;        (f (((lambda (f0)
;          (lambda (x0) (f0 x0))) f) x))))
;
;
;(lambda (f)
;      (lambda (x)
;        (f ((lambda (x0) (f x0)) x))))
;
;(lambda (f)
;      (lambda (x)
;        (f (f x))))
