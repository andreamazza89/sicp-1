(load "little-test.rkt")

(define call-the-cops (lambda () "cops called"))

(define (make-monitored function)
  (define calls-count 0)
  (lambda (input)
    (if (eq? 'how-many-calls? input)
      calls-count
      (begin (set! calls-count (+ calls-count 1))
             (function)))))

(define (make-account balance password cops-function)
  (define attempts 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch try-password m)
    (if (eq? try-password password)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m)))
      (begin
        (if (= 6 attempts)
          (cops-function "too many attempts")
          (set! attempts (+ 1 attempts)))
        (lambda (a) "invalid password"))))
  dispatch)

(define monitored (make-monitored call-the-cops))

(define acc (make-account 100 'secret-password monitored))

((acc 'wrong 'deposit) 50)
((acc 'wrong 'deposit) 50)
((acc 'wrong 'deposit) 50)
((acc 'wrong 'deposit) 50)
((acc 'wrong 'deposit) 50)
((acc 'wrong 'deposit) 50)
((acc 'wrong 'deposit) 50)

(assert-equals "call-the-cops should be called after seven wrong password attempts"
  1
  (monitored 'how-many-calls?))
