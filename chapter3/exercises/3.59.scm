(define (integrate-series series)
  (stream-map / series (integers-from 1)))
