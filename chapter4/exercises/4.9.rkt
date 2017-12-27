(load "little-test.rkt")
(require "chapter4/exercises/the-global-environment.rkt")

;;;;;;;;; eval definition below ;;;;;;;;;;;;;;;;;;;

(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((definition? exp) (eval-definition exp env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
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

(define (definition? exp)
  (tagged-list? exp 'define))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (make-begin seq) (cons 'begin seq))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

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

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (my-eval (definition-value exp) env)
                    env)
  'ok)

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (for->expressions for-exp)
  (make-begin
    (map
      (substitute-item-in-body for-exp (for-binding for-exp))
      (for-collection for-exp))))

(define (for-body for-exp)
  (cadddr for-exp))

(define (for-collection for-exp)
  (caddr for-exp))

(define (for-binding for-exp)
  (cadr for-exp))

(define (substitute-item-in-body for-exp for-binding)
  (lambda (item)
    (map
      (lambda (token) (if (eq? token for-binding) item token))
      (for-body for-exp))))

;;;;;;;;;;;; table of evaluators below ;;;;;;;;;;

(define (eval-for for-exp env)
  (my-eval (for->expressions for-exp) env))

(define (eval-let let-exp env)
  (my-eval (let->combination let-exp) env))

(define (get-let-name let-exp)
  (cadr let-exp))

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
    'let* eval-let*
    'for eval-for))

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

(define (primitive-implementation proc) (mcar (mcdr proc)))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (tagged-list? exp tag)
  (cond
    ((pair? exp)
      (eq? (car exp) tag))
    ((mpair? exp)
      (eq? (mcar exp) tag))
    (else
      false)))

;;;;;;;;;;; tests below ;;;;;;;;;;;;;;;;;;;;;;;;;
; (for [binding-name] [list-of-items] (display [binding-name]))

(assert-equals
  "testing test"
  '(begin (+ 3 1) (+ 3 2))
  (for->expressions '(for item (1 2) (+ 3 item))))

(my-eval '(for my-item ("just" "\n" "checking") (display my-item)) the-global-environment)
