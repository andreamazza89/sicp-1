(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define asdf (list 'a 'b 'c))
(set-cdr! (cdr asdf) asdf)

(define already-seen '())
(define (cycle? x)
  (if (null? (cdr x))
    #f
    (if (memq (cdr x) already-seen)
      #t
      (begin
        (set! already-seen (cons x already-seen))
        (cycle? (cdr x))))))

(cycle? asdf)
