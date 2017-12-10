(load "little-test.rkt")
(require "chapter4/exercises/the-global-environment.rkt")

;;;;;;;;; eval definition below ;;;;;;;;;;;;;;;;;;;

(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((compound-expression? exp)
         ((get-eval-for exp) exp env))
        ((application? exp)
         (my-apply (my-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((boolean? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (compound-expression? exp)
  (hash-has-key? eval-table (car exp)))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (get-eval-for expression)
  (hash-ref eval-table (car expression)))

;;;;;;;;;;;; table of evaluators below ;;;;;;;;;;

(define (eval-let let-exp env)
  (my-eval (let->combination let-exp) env))

(define (let->combination let-exp)
  (cons
      (make-lambda
        (let-variables let-exp)
        (let-body let-exp))
      (let-valuuess let-exp)))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (let-variables let-exp)
  (map
    let-pair-variable
    (let-pairs let-exp)))

(define (let-pairs let-exp)
  (cadr let-exp))

(define (let-pair-variable let-pair)
  (car let-pair))

(define (let-pair-value let-pair)
  (cadr let-pair))

(define (let-body let-exp)
  (cddr let-exp))

(define (let-valuuess let-exp)
  (map
    let-pair-value
    (let-pairs let-exp)))

(define (eval-let* let*-exp env)
  (my-eval (let*->lets let*-exp) env))

(define (let*->lets let*-exp)
  (cond
    ((let-exp-has-only-one-binding let*-exp)
     (let->combination let*-exp))
    (else
      (make-let
        (list (first-binding let*-exp))
        (list (let*->lets (drop-first-binding let*-exp)))))))

(define (let-exp-has-only-one-binding let-exp)
  (= (length (let-variables let-exp)) 1))

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (first-binding let-exp)
  (caadr let-exp))

(define (rest-bindings let-exp)
  (cdadr let-exp))

(define (drop-first-binding let-exp)
  (make-let (rest-bindings let-exp) (let-body let-exp)))

(define (eval-lambda exp env)
  (make-procedure
    (lambda-parameters exp)
    (lambda-body exp)
    env))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (my-eval (first-exp exps) env))
        (else (my-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define eval-table
  (hash
    'lambda eval-lambda
    'let eval-let
    'let* eval-let*))

;;;;;;;;;;; apply definition below ;;;;;;;;;;;;;;

(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define apply-in-underlying-scheme apply)

(define (primitive-implementation proc) (cadr proc))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;;;;;;;;;;; tests below ;;;;;;;;;;;;;;;;;;;;;;;;;

(assert-equals
  "let statement"
   42
   (my-eval '(let ((a 40) (b 2)) (+ a b)) the-global-environment))

(assert-equals
  "nested let statement"
   42
   (my-eval '(let ((a 40))
               (let ((b 2))
                 (+ a b)))
            the-global-environment))

(assert-equals
  "let* statement with only one variable"
  15
  (my-eval '(let* ((x 3)) (* x 5)) the-global-environment))

(assert-equals
  "let* statement with many variables"
  39
  (my-eval '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z)) the-global-environment))
