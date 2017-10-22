;;records package;;
(define make-record cons)
(define get-record-key car)
(define get-record-value cdr)
(define set-record-value! set-cdr!)
;;end of records package;;


;;tables package;;
(define (make-table)
  (list '*table*))
(define get-records cdr)

(define (add-record table record)
  (set-cdr! table (cons record (get-records table))))

(define (find-record-by-key key records)
  (cond ((null? records) #f)
        ((equal? key (get-record-key (car records))) (car records))
        (else (find-record-by-key key (cdr records)))))

(define (insert! table value keys)
  (if (null? (cdr keys))
    (let ((record-found (find-record-by-key (first keys) (get-records table))))
      (if record-found
        (set-record-value! record-found value)
        (add-record table (make-record (first keys) value))))
    (let ((table-found (find-record-by-key (first keys) (get-records table))))
      (cond (table-found (insert! (get-record-value table-found) value (cdr keys)))
            (else
              (let ((new-table (make-table)))
                (add-record table (make-record (first keys) new-table))
                (insert! new-table value (cdr keys))))))))

(define (lookup table keys)
  (if (null? (cdr keys))
    (let ((record-found (find-record-by-key (first keys) (get-records table))))
      (if record-found
        (get-record-value record-found)
        (error "record not found for given keys")))
    (let ((table-found (find-record-by-key (first keys) (get-records table))))
      (if table-found
        (lookup (get-record-value table-found) (cdr keys))
        (error "table not found for given keys")))))
;;end of tables package;;

;;TESTS;;
(display "insert a record under one level of nesting\n")
(define t1 (make-table))
(insert! t1 42 (list 'meaning-of-life))
(display (= 42 (lookup t1 (list 'meaning-of-life))))
(newline)

(display "insert two records under one level of nesting\n")
(define t2 (make-table))
(insert! t2 42 (list 'meaning-of-life))
(insert! t2 33 (list 'another-number))
(display (= 42 (lookup t2 (list 'meaning-of-life))))
(newline)
(display (= 33 (lookup t2 (list 'another-number))))
(newline)

(display "overwrite a record under one level of nesting\n")
(define t3 (make-table))
(insert! t3 42 (list 'meaning-of-life))
(insert! t3 33 (list 'meaning-of-life))
(display (= 33 (lookup t3 (list 'meaning-of-life))))
(newline)

(display "insert two records under two levels of nesting\n")
(define t4 (make-table))
(insert! t4 42 (list 'meaning-of-life 'giorgio))
(insert! t4 555 (list 'meaning-of-life 'mario))
(display (= 42 (lookup t4 (list 'meaning-of-life 'giorgio))))
(newline)
(display (= 555 (lookup t4 (list 'meaning-of-life 'mario))))
(newline)

(display "more nesting\n")
(define t5 (make-table))
(insert! t5 42 (list 'meaning-of-life 'giorgio))
(insert! t5 555 (list 'meaning-of-life 'mario))
(insert! t5 666 (list 'cicciput 'mario 'darly))
(insert! t5 123 (list 'cicciput 'mario 'asdf))
(insert! t5 234 (list 'cicciput 'mario 'ff 'foo))
(display (= 42 (lookup t5 (list 'meaning-of-life 'giorgio))))
(newline)
(display (= 555 (lookup t5 (list 'meaning-of-life 'mario))))
(newline)
(display (= 666 (lookup t5 (list 'cicciput 'mario 'darly))))
(newline)
(display (= 123 (lookup t5 (list 'cicciput 'mario 'asdf))))
(newline)
(display (= 234 (lookup t5 (list 'cicciput 'mario 'ff 'foo))))
