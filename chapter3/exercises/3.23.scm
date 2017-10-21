;;node package; a node has a value, as well as a before and after pointers;;
(define (make-node item before after)
  (cons item (cons before after)))

(define (get-item node)
  (car node))

(define (get-before node)
  (cadr node))
(define (set-before! this-node before-node)
  (set-car! (cdr this-node) before-node))

(define (get-after node)
  (cddr node))
(define (set-after! this-node after-node)
  (set-cdr! (cdr this-node) after-node))
;;end of node package

;;; start of deque package;;;;
(define (make-deque)
  (cons '() '()))

(define (empty-deque? deque)
  (null? (car deque)))

(define (front-node deque)
  (car deque))
(define (set-front-node! deque item)
  (set-car! deque item))
(define (rear-node deque)
  (cdr deque))
(define (set-rear-node! deque item)
  (set-cdr! deque item))

(define (initialise-empty-deque deque item)
  (let ((new-node (make-node item '() '())))
    (set-front-node! deque new-node)
    (set-rear-node! deque new-node)))

(define (front-deque deque)
  (get-item (front-node deque)))

(define (front-insert-deque! deque item)
  (cond
    ((empty-deque? deque) (initialise-empty-deque deque item))
    (else
      (let ((new-front-node (make-node item '() (front-node deque))))
        (set-before! (front-node deque) new-front-node)
        (set-front-node! deque new-front-node)))))

(define (front-delete-deque! deque)
  (let ((second-node (get-after (front-node deque))))
    (cond
      ((null? second-node)
       (set-front-node! deque '())
       (set-rear-node! deque '()))
      (else
        (set-before! second-node '())
        (set-front-node! deque second-node)))))

(define (rear-deque deque)
  (get-item (rear-node deque)))

(define (rear-insert-deque! deque item)
  (cond
    ((empty-deque? deque) (initialise-empty-deque deque item))
    (else
      (let ((new-rear-node (make-node item (rear-node deque) '())))
        (set-after! (rear-node deque) new-rear-node)
        (set-rear-node! deque new-rear-node)))))

(define (rear-delete-deque! deque)
  (let ((penultimate-node (get-before (rear-node deque))))
    (cond
      ((null? penultimate-node)
       (set-front-node! deque '())
       (set-rear-node! deque '()))
      (else
        (set-after! penultimate-node '())
        (set-rear-node! deque penultimate-node)))))

;;;;TESTS;;;;
(display "a deque is created empty")
(define d1 (make-deque))
(empty-deque? d1)
(display "^^^^^^^^^^^^^end test^^^^^^^^^^^^^^")

(display "you can add an element to the front of the deque")
(define d2 (make-deque))
(front-insert-deque! d2 42)
(= 42 (front-deque d2))
(display "^^^^^^^^^^^^^end test^^^^^^^^^^^^^^")

(display "you can add an element to the rear of the deque")
(define d3 (make-deque))
(rear-insert-deque! d3 55)
(= 55 (rear-deque d3))
(display "^^^^^^^^^^^^^end test^^^^^^^^^^^^^^")

(display "with only one element in the queue, front and rear are the same (front insert)")
(define d4 (make-deque))
(front-insert-deque! d4 99)
(= 99 (front-deque d4))
(= 99 (rear-deque d4))
(display "^^^^^^^^^^^^^end test^^^^^^^^^^^^^^")

(display "a deque with an item is not empty")
(define d5 (make-deque))
(front-insert-deque! d5 98)
(eq? #f (empty-deque? d5))
(display "^^^^^^^^^^^^^end test^^^^^^^^^^^^^^")

(display "adding a second element at the front")
(define d6 (make-deque))
(front-insert-deque! d6 11)
(front-insert-deque! d6 22)
(= 22 (front-deque d6))
(= 11 (rear-deque d6))
(display "^^^^^^^^^^^^^end test^^^^^^^^^^^^^^")

(display "adding a second element at the rear")
(define d7 (make-deque))
(rear-insert-deque! d7 123)
(rear-insert-deque! d7 22)
(= 123 (front-deque d7))
(= 22 (rear-deque d7))
(display "^^^^^^^^^^^^^end test^^^^^^^^^^^^^^")

(display "deleting from the rear of a 1-element queue")
(define d8 (make-deque))
(rear-insert-deque! d8 123)
(rear-delete-deque! d8)
(empty-deque? d8)
(display "^^^^^^^^^^^^^end test^^^^^^^^^^^^^^")

(display "deleting from the rear of a 2-element queue")
(define d9 (make-deque))
(rear-insert-deque! d9 123)
(rear-insert-deque! d9 999)
(rear-insert-deque! d9 535)
(rear-delete-deque! d9)
(= 999 (rear-deque d9))
(= 123 (front-deque d9))
(display "^^^^^^^^^^^^^end test^^^^^^^^^^^^^^")

(display "deleting from the front of a 2-element queue")
(define d10 (make-deque))
(rear-insert-deque! d10 123)
(rear-insert-deque! d10 999)
(rear-insert-deque! d10 535)
(front-delete-deque! d10)
(= 535 (rear-deque d10))
(= 999 (front-deque d10))
(display "^^^^^^^^^^^^^end test^^^^^^^^^^^^^^")
