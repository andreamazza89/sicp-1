(define (ripple-carry-adder as bs ss final-carry)
  (define (iter as bs ss previous-mid-carry)
    (cond
      ((null? (cdr as))
       (full-adder (car as) (car bs) (car ss) previous-mid-carry final-carry))
      (else
        (let ((next-mid-carry (make-wire)))
          (full-adder (car as) (car bs) (car ss) previous-mid-carry next-mid-carry)
          (iter (cdr as) (cdr bs) (cdr ss) next-mid-carry)))))
  (iter as bs ss (make-wire)))
