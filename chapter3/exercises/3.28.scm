(define (make-wire)
  (cons (lambda () (lambda () 'ok)) 0))

(define get-signal cdr)

(define (set-signal! wire value)
  (set-cdr! wire value)
  (call-action wire))

(define (add-action! wire func)
  (set-car! wire func))

(define (call-action wire)
  (((car wire))))

(define (logical-or a b)
  (if (or (= 1 a) (= 1 b)) 1 0))

(define (or-gate in1 in2 out1)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal in1) (get-signal in2))))
      (lambda () (set-signal! out1 new-value))))
  (add-action! in1 or-action-procedure)
  (add-action! in2 or-action-procedure)
  'ok)



;;TESTS;;
(display "simple or gate\n")
(define in1 (make-wire))
(define in2 (make-wire))
(define out1 (make-wire))
(or-gate in1 in2 out1)
(set-signal! in1 1)
(display (= 1 (get-signal out1)))
