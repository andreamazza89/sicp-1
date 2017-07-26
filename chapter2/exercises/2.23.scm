(define (my-for-each proc elements)
  (if (null? elements)
    "done"
    (let ((side-effect (proc (car elements))))
      (my-for-each proc (cdr elements)))))

(my-for-each
  display
  (list "yololo\n" "yolola\n" "trololo\n"))
