(load "little-test.rkt")

(define (make-account balance password)
  (define passwords (list password))

  (define (add-password new-pass) (set! passwords (cons new-pass passwords)))

  (define (password-correct? try-password)
    (and (member try-password passwords) #t))

  (define (get-balance)
    balance)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch try-password m)
    (if (password-correct? try-password)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'check-password) password-correct?)
            ((eq? m 'add-password) add-password)
            ((eq? m 'balance) balance)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m)))
      (lambda (_) "incorrect password")))
  dispatch)

(define (make-joint account-to-clone password-of-acc-to-clone password-for-new-account)
    ((account-to-clone password-of-acc-to-clone 'add-password) password-for-new-account)
    account-to-clone)

(define paul-acc (make-account 100 'secret-password))
(assert-equals "can deposit funds with correct password"
  150
  ((paul-acc 'secret-password 'deposit) 50))

(assert-equals "does not allow deposit with wrong password"
  "incorrect password"
  ((paul-acc 'wrong-password 'deposit) 50))

(assert-equals "account was unaffected by transaction with wrong password"
  150
  (paul-acc 'secret-password 'balance))

(define giorgio-acc (make-joint paul-acc 'secret-password 'giorgios-secret))
(assert-equals "creates joint account"
  150
  (giorgio-acc 'giorgios-secret 'balance))

(define broken-account (make-joint paul-acc 'asdfsaf 'new-password))
(assert-equals "cannot create new account without right password"
  "incorrect password"
  ((broken-account 'new-password 'deposit) 4))
