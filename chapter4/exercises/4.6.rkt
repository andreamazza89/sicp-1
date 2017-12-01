(define (let->combination let-exp env)
  (my-apply
    (make-procedure
      (get-let-parameters let-exp)
      (get-let-body let-exp)
      env)
    (get-let-parameters-values let-exp)))

(define (get-let-parameters let-exp)
  (map
    car
    (get-let-pairs let-exp)))

(define (get-let-parameters-values let-exp)
  (map
    cadr
    (get-let-pairs let-exp)))

(define (get-let-parameters-values let-exp)

(define (get-let-body let-exp)
  (caddr let-exp))

(define (get-let-pairs let-exp)
  (cadr let-exp))
