(load "little-test.rkt")

(define (reverse-l sequence)
  (foldl (lambda (item memo) (cons item memo)) '() sequence))

(define (reverse-r sequence)
  (foldr (lambda (item memo) (append memo (list item))) '() sequence))

(assert-equals "reverse using foldl"
  '(5 4 3 2 1)
   (reverse-l '(1 2 3 4 5)))

(assert-equals "reverse using foldr"
  '(5 4 3 2 1)
   (reverse-r '(1 2 3 4 5)))
