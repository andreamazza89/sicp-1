(load "little-test.rkt")

(define (make-vec x y)
  (cons x y))

(define (xcor-vec vec)
  (car vec))

(define (ycor-vec vec)
  (cdr vec))

(define (add-vect vect1 vect2)
  (make-vec
    (+ (xcor-vec vect1) (xcor-vec vect2))
    (+ (ycor-vec vect1) (ycor-vec vect2))))

(define (sub-vect vect1 vect2)
  (make-vec
    (- (xcor-vec vect1) (xcor-vec vect2))
    (- (ycor-vec vect1) (ycor-vec vect2))))

(define (scale-vect vect scalar)
  (make-vec (* scalar (xcor-vec vect)) (* scalar (ycor-vec vect))))

(assert-equals "adds vectors"
  (make-vec 5 8)
  (add-vect (make-vec 2 4) (make-vec 3 4)))

(assert-equals "subtracts vectors"
  (make-vec 4 4)
  (sub-vect (make-vec 14 10) (make-vec 10 6)))

(assert-equals "scales vectors"
  (make-vec 4 10)
  (scale-vect (make-vec 2 5) 2))
