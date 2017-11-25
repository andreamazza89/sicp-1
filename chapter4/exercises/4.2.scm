; a: the problem is that the application predicate simply checks whether the given
; expression is a pair, as it relies on the fact that all other predicates have failed
; So if this is moved up before assigment, somethin like (define x 3) will be
; treated as function application. The symbol 'define will be evaluated, but it will
; not be found within the environment as a procedure?

; b
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (application? exp)
  (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
