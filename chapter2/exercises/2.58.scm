(load "little-test.rkt")

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (list m1 '* m2))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (make-sum s1 s2)
  (list s1 '+ s2))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))


(assert-equals "supports standard notation for addition"
  '(a + 4)
  (make-sum 'a 4))

(assert-equals "selects the addend of an addition"
  'c
  (addend '(c + a)))

(assert-equals "selects the augend of an addition"
  'p
  (augend '(d + p)))

(assert-equals "recognises a sum"
 #t
 (sum? '(a + b)))

(assert-equals "supports standard notation for multiplication"
  '(b * c)
  (make-product 'b 'c))

(assert-equals "selects the multiplier of a multiplication"
  'r
  (multiplier '(r * 5)))

(assert-equals "selects the multiplicand of a multiplication"
  'e
  (multiplicand '(x * e)))

(assert-equals "recognises a multiplication"
  #t
  (product? '(d * x)))

(assert-equals "derivative in standard notation"
  '((x * y) + (y * (x + 3)))
  (deriv '((x * y) * (x + 3)) 'x))
