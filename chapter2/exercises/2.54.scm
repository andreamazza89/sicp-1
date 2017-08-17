(load "little-test.rkt")

(define (my-equal? list-one list-two)
  (cond
    ((and (null? list-one) (null? list-two)) #t)
    ((or (null? list-one) (null? list-two)) #f)
    ((eq? (car list-one) (car list-two)) (my-equal? (cdr list-one) (cdr list-two)))
    (#t #f)))

(assert-equals "two equal lists"
  #t
  (my-equal? '(I am equal) '(I am equal)))

(assert-equals "two different lists"
  #f
  (my-equal? '(I '(am not) equal) '(I dont know anymore)))
