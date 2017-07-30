(load "little-test.rkt")

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector matrix vector)
     (map (lambda (other-vector) (dot-product vector other-vector)) matrix))

(define (transpose matrix)
    (accumulate-n (lambda (dot vector) (cons dot vector)) '() matrix))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
          (map (lambda (row) (matrix-*-vector cols row)) m)))

(assert-equals "matrix times vector"
  (list 13 31 49)
  (matrix-*-vector (list (list 1 2 3) (list 4 5 6) (list 7 8 9))
                   (list 2 1 3)))

(assert-equals "transpose a matrix"
  (list (list 1 4 7) (list 2 5 8) (list 3 6 9))
  (transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9))))

(assert-equals "transpose another matrix"
  (list (list 1) (list 2))
  (transpose (list (list 1 2))))

(assert-equals "matrices multiplication"
  (list (list 4 4) (list 10 8))
  (matrix-*-matrix (list (list 1 2) (list 3 4))
                   (list (list 2 0) (list 1 2))))
