(define (make-queue)
  (define front-pointer '())
  (define rear-pointer '())

  (define (delete-queue!)
    (set! front-pointer (cdr front-pointer)))
	(define (empty-queue?)
    (if (pair? front-pointer)
      (null? (car front-pointer))
      #t))
	(define (front-queue)
		(car front-pointer))
  (define (insert-queue! item)
    (let ((new-item (cons item '())))
      (cond
        ((empty-queue?)
          (set! front-pointer new-item)
          (set! rear-pointer new-item))
        (else
          (set-cdr! rear-pointer new-item)
          (set! rear-pointer new-item)))))

	(define (dispatch function-name)
		(cond
			((eq? 'delete-queue! function-name) delete-queue!)
			((eq? 'empty-queue? function-name) empty-queue?)
			((eq? 'insert-queue! function-name) insert-queue!)
			((eq? 'front-queue function-name) front-queue)))
	dispatch)

;;;TEST;;;;
(display "the queue is empty as soon as it's created")
(define q1 (make-queue))
((q1 'empty-queue?))

(display "there is nothing in the queue as soon as it's created")
(define q2 (make-queue))
(null? ((q2 'front-queue)))

(display "adding an item to the queue")
(define q3 (make-queue))
((q3 'insert-queue!) 42)
(= 42 ((q3 'front-queue)))

(display "deleting an item in a one item queue leaves it empty")
(define q4 (make-queue))
((q4 'insert-queue!) 332)
((q4 'delete-queue!))
((q4 'empty-queue?))

(display "deleting an item in a queue with many items")
(define q5 (make-queue))
((q5 'insert-queue!) 123)
((q5 'insert-queue!) 333)
((q5 'insert-queue!) 532)
((q5 'delete-queue!))
(= 333 ((q5 'front-queue)))
