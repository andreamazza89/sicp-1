
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

(define (logical-and a b)
  (if (and (= 1 a) (= 1 b)) 1 0))

(define (logical-not a)
  (if (= a 0) 1 0))

(define (and-gate in1 in2 out1)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal in1) (get-signal in2))))
      (lambda () (set-signal! out1 new-value))))
  (add-action! in1 and-action-procedure)
  (add-action! in2 and-action-procedure)
  'ok)

(define (not-gate in1 out1)
  (define (not-action-procedure)
    (let ((new-value (logical-not (get-signal in1))))
      (lambda () (set-signal! out1 new-value))))
  (add-action! in1 not-action-procedure)
  'ok)

(define (or-gate in1 in2 out1)
  (let ((to-and1 (make-wire)) (to-and2 (make-wire)) (to-not (make-wire)))
    (not-gate in1 to-and1)
    (not-gate in2 to-and2)
    (and-gate to-and1 to-and2 to-not)
    (not-gate to-not out1)))

; NOT((NOT x) AND (NOT y))

;;TESTS;;
(display "simple or gate\n")
(define in1 (make-wire))
(define in2 (make-wire))
(define out1 (make-wire))
(or-gate in1 in2 out1)
(set-signal! in1 1)
(display (= 1 (get-signal out1)))
(newline)

;;TESTS;;
(display "simple or gate\n")
(define in11 (make-wire))
(define in21 (make-wire))
(define out11 (make-wire))
(or-gate in11 in21 out11)
(set-signal! in21 1)
(display (= 1 (get-signal out11)))
(newline)

;;TESTS;;
(display "simple or gate\n")
(define in12 (make-wire))
(define in22 (make-wire))
(define out12 (make-wire))
(or-gate in12 in22 out12)
(display (= 0 (get-signal out12)))
(newline)

;;TESTS;;
(display "simple or gate\n")
(define in13 (make-wire))
(define in23 (make-wire))
(define out13 (make-wire))
(or-gate in13 in23 out13)
(set-signal! in13 1)
(set-signal! in23 1)
(display (= 1 (get-signal out13)))
