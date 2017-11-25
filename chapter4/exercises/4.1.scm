(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (list-of-values-lr exps env)
  (let ((first-value (eval (first-operand exps) env)))
    (if (no-operands? exps)
          '()
          (cons first-value
                (list-of-values (rest-operands exps) env)))))

(define (list-of-values-rl exps env)
  (let ((rest-values (list-of-values (rest-operands exps) env)))
    (if (no-operands? exps)
          '()
          (cons (eval (first-operand exps) env)
                rest-values))))

; this is assuming the let statement actually evaluates the new bindings before
; evaluating the body of the let. Otherwise it might need nested lets.
