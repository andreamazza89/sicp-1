(load "little-test.rkt")

; mobile
(define (make-mobile left right)
  (cons left right))
(define left-branch car)
(define right-branch cdr)

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (balanced-mobile? mobile)
  (let ((Lbranch (left-branch mobile))
        (Rbranch (right-branch mobile)))
    (and (= (* (branch-length Lbranch) (branch-weight Lbranch))
            (* (branch-length Rbranch) (branch-weight Rbranch)))
         (balanced-branch? Lbranch)
         (balanced-branch? Rbranch))))

; branch
(define (make-branch length structure)
    (cons length structure))

(define branch-length car)
(define branch-structure cdr)

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (number? structure)
      structure
      (total-weight structure))))

(define (balanced-branch? branch)
  (let ((structure (branch-structure branch)))
    (if (number? structure)
      #t
      (balanced-mobile? structure))))

;;;;;;;;
;; tests
;;;;;;;;

(assert-equals "gets the left branch"
  "left branch"
  (left-branch (make-mobile "left branch" "right branch")))

(assert-equals "gets the left branch example two"
  "L branch"
  (left-branch (make-mobile "L branch" "right branch")))

(assert-equals "gets the right branch"
  "right branch"
  (right-branch (make-mobile "left branch" "right branch")))

(assert-equals "gets the right branch example two"
  "R branch"
  (right-branch (make-mobile "left branch" "R branch")))

(assert-equals "gets the branch length"
  33
  (branch-length (make-branch 33 21)))

(assert-equals "gets the branch structure"
  42
  (branch-structure (make-branch 33 42)))

(define branch-zero (make-branch 220 1))
(define branch-one (make-branch 22 10))
(define branch-two (make-branch 12 32))
(define simple-mobile (make-mobile branch-one branch-two))
(define branch-three (make-branch 10 simple-mobile))
(define complex-mobile (make-mobile branch-one branch-three))
(define balanced-simple-mobile (make-mobile branch-zero branch-one))
(define branch-four (make-branch 20 balanced-simple-mobile))
(define balanced-complex-mobile (make-mobile branch-zero branch-four))
(assert-equals "calculates the weight of a simple mobile"
  42
  (total-weight simple-mobile))

(assert-equals "calculates the weight of a complex mobile"
  52
  (total-weight complex-mobile))

(assert-equals "knows if a simple mobile is not balanced"
  #f
  (balanced-mobile? simple-mobile))

(assert-equals "knows if a simple mobile is balanced"
  #t
  (balanced-mobile?  balanced-simple-mobile))

(assert-equals "knows if a complex mobile is balanced"
  #t
  (balanced-mobile? balanced-complex-mobile))
