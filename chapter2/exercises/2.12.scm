(define (make-center-percent center percentage)
  (make-interval (get-percentage-lower-bound center percentage)
                 (get-percentage-upper-bound center percentage)))

(define (get-percentage-lower-bound center percentage)
  (- center (* (/ center 100) percentage)))

(define (get-percentage-upper-bound center percentage)
  (+ center (* (/ center 100) percentage)))

(define (percent interval)
  (* (/ (width interval)
        (center interval)
     100)))
