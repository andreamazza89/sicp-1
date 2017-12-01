(load "little-test.rkt")

(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((compound-expression? exp)
         ((get-eval-for exp) exp env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((boolean? exp) true)
        (else false)))

(define compound-expression? list?)

(define (get-eval-for expression)
  (hash-ref eval-table (car expression)))

;;;;;;;;;;;; table of evaluators below ;;;;;;;;;;

(define (eval-and exp env)
  (let ([evaluated (evaluate-each (cdr exp) env)])
    (cond ((null? (cdr exp)) #t)
          ((member #f evaluated) #f)
          (else (last evaluated))))))

(define (eval-or exp env)
  (let* ([evaluated (evaluate-each (cdr exp) env)]
         [first-not-false (findf (lambda (i) (not (eq? #f i))) evaluated)])
    (cond ((null? (cdr exp)) #f)
          (first-not-false first-not-false)
          (else #f))))

(define (evaluate-each expressions env)
  (map
    (lambda (exp) (my-eval exp env))
    expressions))

(define eval-table
  (hash
    'or eval-or
    'and eval-and))

;;;;;;;;;;; tests below ;;;;;;;;;;;;;;;;;;;;;;;;;

(assert-equals
  "AND with no expressions"
   #t
   (my-eval '(and) '()))

(assert-equals
  "AND with no false expression"
   42
   (my-eval '(and 41 42) '()))

(assert-equals
  "AND with at least one false expression"
   #f
   (my-eval '(and 55 #t #f 42) '()))


(assert-equals
  "OR with no expressions"
   #f
   (my-eval '(or) '()))

(assert-equals
  "OR with a non-false expression"
   42
   (my-eval '(or #f 42) '()))

(assert-equals
  "OR with all false expressions"
   #f
   (my-eval '(or #f #f) '()))
