(load "little-test.rkt")

(define (make-account balance password)

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
      (error "incorrect password")))
  dispatch)

(define acc (make-account 100 'secret-password))
(assert-equals "can deposit funds with correct password"
  150
  ((acc 'secret-password 'deposit) 50))
