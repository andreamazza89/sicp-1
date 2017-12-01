(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((compound-expression? exp)
         ((get (car exp)) (cdr exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define compound-expression? list?)

; imagine all necessary procedures are installed in the table so they
; can be retrieved with get
