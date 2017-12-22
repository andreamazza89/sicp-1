#lang racket

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (mcons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-mcar! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (mcar vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (enclosing-environment env) (mcdr env))

(define (frame-variables frame) (mcar frame))

(define (frame-values frame) (mcdr frame))

(define (first-frame env) (mcar env))

(define (make-frame variables values)
  (mcons variables values))

(define the-empty-environment '())

(define the-global-environment
  (extend-environment
    (list '+ '*)
    (list (list 'primitive +) (list 'primitive *))
    the-empty-environment))

(provide define-variable!)
(provide extend-environment)
(provide lookup-variable-value)
(provide the-global-environment)
