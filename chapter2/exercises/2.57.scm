(load "little-test.rkt")

(define (in-seq? proc seq)
  (if (memf proc seq)
    #t
    #f))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum summers)
  (let ((summs (filter-not (lambda (x) (=number? x 0)) summers)))
    (cond
      ((null? summs) '())
      ((null? (cdr summs)) (car summs))
      ((andmap number? summs) (foldl + 0 summs))
      (else (append (list '+) summs)))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product multipliers)
  (let ((ms (filter-not (lambda (x) (=number? x 1)) multipliers)))
    (cond
      ((null? ms) '())
      ((null? (cdr ms)) (car ms))
      ((in-seq? (lambda (x) (=number? x 0)) ms) 0)
      (else (append (list '*) ms)))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (make-product (cdr (cdr p))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (make-sum (cdr (cdr s))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (make-exponentiation base exponent)
  (cond
    ((=number? exponent 1) base)
    ((=number? base 1) 1)
    (list '** base exponent)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (list (deriv (addend exp) var)
                   (deriv (augend exp) var))))
        ((product? exp)
         (make-sum
           (list (make-product (list (multiplier exp)
                         (deriv (multiplicand exp) var)))
           (make-product (list (deriv (multiplier exp) var)
                         (multiplicand exp))))))
        ((exponentiation? exp)
         (make-product (list
                         (exponent exp)
                         (make-exponentiation (base exp) (make-sum (list (exponent exp) -1)))
                         (deriv (base exp) (base exp)))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(assert-equals "supports multiple multiplications"
  '(+ (* x y) (* y (+ x 3)))
  (deriv '(* x y (+ x 3)) 'x))

(assert-equals "supports multiple additions"
  '(+ 1 (* 2 x))
  (deriv '(+ x y (** x 2)) 'x))
