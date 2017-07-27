(define (print-colour message colour)
  (fprintf (current-output-port) "~a~a" (integer->char #x1b) (format "[~a;~a~a~am" "49" colour "" ""))
  (println message (current-output-port))
  (fprintf (current-output-port) "~a~a" (integer->char #x1b) "[0m"))

(define (print-green message)
  (print-colour message "32"))

(define (print-red message)
  (print-colour message "31"))

(define (test-failure-message description expected given)
  (string-append
    description
    " ---> "
    "Expected: " expected ", given: " given))

(define (assert-equals description expected given)
  (newline)
  (if (equal? expected given)
    (print-green description)
    (print-red (test-failure-message description (~v expected) (~v given)))))
