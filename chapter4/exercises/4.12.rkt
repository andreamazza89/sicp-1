(load "little-test.rkt")
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
    (mlist '+ '* 'display)
    (mlist (mlist 'primitive +) (mlist 'primitive *) (mlist 'primitive display))
    the-empty-environment))

(define test1-env the-global-environment)
(define-variable! 'test-var 42 test1-env)
(assert-equals
  "defines a variable that does not exist"
  42
  (lookup-variable-value 'test-var test1-env))

(define-variable! 'test-var 33 test1-env)
(assert-equals
  "defines a variable that already exists"
  33
  (lookup-variable-value 'test-var test1-env))

(set-variable-value! 'test-var 66 test1-env)
(assert-equals
  "sets a variable in the nearest frame"
  66
  (lookup-variable-value 'test-var test1-env))

(define test2-env
  (extend-environment
    (mlist 'test2-var)
    (mlist 'hello)
    test1-env))
(set-variable-value! 'test-var 'can-you-hear-me test2-env)
(assert-equals
  "sets a variable one frame down"
  'can-you-hear-me
  (lookup-variable-value 'test-var test2-env))
