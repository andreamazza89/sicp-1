(define (inc n)
  (+ n 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpsons-parameter k n)
  (cond
    ((or (= k 0) (= k n)) 1)
    ((even? k) 2)
    (else 4)))

(define (simpsons f a b n)
  (define h (/ (- b a) n))
  (define (simpsons-term k)
    (* (f (+ a (* k h)))
       (simpsons-parameter k n)))
  (*
    (/ h 3)
    (sum simpsons-term 0 inc n)))

;;;;;;;;;;;;;;;;;;;;;;;;;

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

(define (cube x)
  (* x x x))

(simpsons cube 0 1 100)
(simpsons cube 0 1 1000)
(integral cube 0 1 0.01)
(integral cube 0 1 0.0001)
