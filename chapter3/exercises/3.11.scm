(define acc (make-account 50))
; this binds whatever (make-account 50) returns, which is the define procedure, in the global scope G to the symbol acc.
; evaluating (make-account 50) creates a new environment E1, in which balance is bound to 50. The acc procedure in the global
; scope points at this environment.

; G (acc->dispatch)  E1(balance->50) | dispatch points at E1

((acc 'deposit) 40)
; evaluating acc creates a new environment E2, which points at E1 and in which m is set to 'deposit.
; this returns the deposit procedure, which is then evaluated, creating E3, pointing at E1 and with amount set to 40.

; G (acc->dispatch)  E1(balance->90) E2(m->'deposit) E3(amount->40) | E2 and E3 point at E1

((acc 'withdraw) 60)
; evaluating acc creates a new environment E4, which points at E1 and in which m is set to 'withdraw
; this returns the withdraw procedure, which is then evaluated, creating E5, pointing at E1 and with amount set to 60

; G (acc->dispatch)  E1(balance->30) E2(m->'deposit) E3(amount->40) E4(m->'withdraw) E5(amount->60)| E2, E3, E4, E5 point at E1



; creating acc2 would bind the dispatch procedure to 'acc2 in the global environment, however this would point to a new
; environment with balance set to 100.

; the only shared parts of the environment structure that are shared are the global environment
