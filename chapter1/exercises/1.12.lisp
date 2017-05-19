(define (factorial n)
  (cond
    [(= 0 n) 1]
    [(* n (factorial (- n 1)))]))

(define (binomial-coefficient row choose)
  (cond
    [(and (< choose row) (<= 0 choose))
      (/
        (factorial row)
        (* (factorial (- row choose)) (factorial choose)))]
    [else 1]))

(define (pascals-row row)
  (do-pascals-row row '() row))

(define (do-pascals-row row-index row-items column-index)
  (cond
    [(= column-index 0) (cons 1 row-items)]
    [else (do-pascals-row
            row-index
            (cons (binomial-coefficient row-index column-index) row-items)
            (- column-index 1))]))

(pascals-row 0)
(pascals-row 1)
(pascals-row 2)
(pascals-row 3)
(pascals-row 4)
(pascals-row 5)
(pascals-row 6)
(pascals-row 7)
