#lang racket

(require compatibility/mlist)

(define (extend-environment vars vals base-env)
  (if (= (mlength vars) (mlength vals))
      (mcons (make-frame vars vals) base-env)
      (if (< (mlength vars) (mlength vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (env-loop env target-variable error-message found-procedure)
  (define (loop env)
    (if (eq? env the-empty-environment)
      (error error-message target-variable)
    (scan
      (first-frame env)
      target-variable
      (lambda () (loop (enclosing-environment env)))
      (lambda (vals) (found-procedure vals)))))
  (loop env))

(define (scan frame target-variable not-found-procedure found-procedure)
  (define (loop variables valuess)
    (cond ((null? variables)
            (not-found-procedure))
          ((eq? target-variable (mcar variables))
            (found-procedure valuess))
          (else
            (loop (mcdr variables) (mcdr valuess)))))
  (loop (frame-variables frame) (frame-values frame)))

(define (define-variable! var val env)
  (scan
    (first-frame env)
    var
    (lambda () (add-binding-to-frame! var val (first-frame env)))
    (lambda (vals) (set-mcar! vals val))))

(define (set-variable-value! var val env)
  (env-loop
    env
    var
    "Unboud variable -- SET!"
    (lambda (vals) (set-mcar! vals val))))

(define (lookup-variable-value var env)
  (env-loop
    env
    var
    "Unbound variable"
    (lambda (vals) (mcar vals))))

(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

(define (enclosing-environment env) (mcdr env))
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define (first-frame env) (mcar env))

(define (make-frame variables values)
  (mcons variables values))

(define the-empty-environment '())

(define the-global-environment
  (extend-environment
    (mlist '+ '* 'display 'list 'map)
    (mlist (mlist 'primitive +)
           (mlist 'primitive *)
           (mlist 'primitive display)
           (mlist 'primitive list)
           (mlist 'primitive map))
    the-empty-environment))

(provide lookup-variable-value)
(provide the-global-environment)
