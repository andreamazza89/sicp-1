(load "little-test.rkt")

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (make-record key contents)
  (cons key contents))
(define (get-key record)
  (car record))

(define (lookup key record-set)
  (if (null? record-set)
    #f
    (let ((current-key (get-key (entry record-set))))
      (cond
        ((= key current-key) (entry record-set))
        ((< key current-key) (lookup key (left-branch record-set)))
        (else (lookup key (right-branch record-set)))))))


(assert-equals "returns false when record does not exist"
  #f
  (lookup 5 (make-tree (make-record 4 "record contents") '() '())))

(define test-records
  (make-tree
    (make-record 5 "I am record with ID 5")
    (make-tree
      (make-record 3 "I am record with ID 3")
      '()
      '())
    (make-tree
      (make-record 7 "I am record with ID 7")
      '()
      (make-tree
        (make-record 11 "I am record with ID 11")
        '()
        '()))))
(assert-equals "finds a record"
  (make-record 11 "I am record with ID 11")
  (lookup 11 test-records))
