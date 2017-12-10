#lang racket

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (enclosing-environment env) (cdr env))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (first-frame env) (car env))

(define (make-frame variables values)
  (cons variables values))

(define the-empty-environment '())

(define the-global-environment
  (extend-environment
    (list '+ '*)
    (list (list 'primitive +) (list 'primitive *))
    the-empty-environment))

(provide extend-environment)
(provide lookup-variable-value)
(provide the-global-environment)
