;;;;;;;;;;;; eval definition below ;;;;;;;;;;
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

(define (get-eval-for expression)
  (hash-ref eval-table (car expression)))

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

;;;;;;;;;;;; table of evaluators below ;;;;;;;;;;

(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))

(define (true? x)
  (not (eq? x #f)))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (eval-cond exp env)
  (my-eval (cond->if exp) env))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (if (is-arrow-cond? first)
                       (list (arrow-proc first) (cond-predicate first))
                       (sequence->exp (cond-actions first)))
                     (expand-clauses rest))))))

(define (cond-clauses exp) (cdr exp))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))


(define (eval-sequence exps env)
  (cond ((last-exp? exps) (my-eval (first-exp exps) env))
        (else (my-eval (first-exp (cdr exps)) env)
              (eval-sequence (rest-exps (cdr exps)) env))))

(define (make-begin seq) (cons 'begin seq))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))


(define (make-procedure exp env)
  (list 'procedure (cadr exp) (cddr exp) env))



(define eval-table
  (hash
    'and eval-and
    'begin eval-sequence
    'cond eval-cond
    'if eval-if
    'lambda make-procedure
    'or eval-or))

;;;;;;;;;;; tests below ;;;;;;;;;;;;;;;;;;;;;;;;;
(define myenv (extend-environment '(+) (list +) the-empty-environment))
(assert-equals
  "evaluates a lambda"
   66
   (my-eval '(+ 1 2) myenv))

;(assert-equals
;  "evaluates a lambda"
;   66
;   (my-eval '((lambda (n) (+ n 1)) 65) '()))
