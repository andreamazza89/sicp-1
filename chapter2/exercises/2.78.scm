(load "little-test.rkt")

(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond
    ((pair? datum) (car datum))
    ((number? datum) 'scheme-number)
    (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond
    ((pair? datum) (cdr datum))
    ((number? datum) datum)
    (else (error "Bad tagged datum -- CONTENTS" datum))))

;;;;;;;;;

(assert-equals "recognises standard numbers"
  'scheme-number
  (type-tag (attach-tag 'scheme-number 55)))

(assert-equals "recognises tagged numbers"
  'tagged-number
  (type-tag (attach-tag 'tagged-number (list 1 2 3))))

(assert-equals "extracts contents of standard number"
  42
  (contents 42))

(assert-equals "extract contents of tagged numbers"
  '(1 22 33)
  (contents (attach-tag 'tagged-number (list 1 22 33))))
