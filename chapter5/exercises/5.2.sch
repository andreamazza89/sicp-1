; should have renamed t1 as new-product and t2 as new-counter, but
; cannot be asked to change the diagram of excercise 5.1

(controller
  test-counter
    (test (op >) (reg counter) (register n))
    (branch (label factorial-done))
    (assign t1 (op *) (reg product) (reg counter))
    (assign t2 (op +) (const 1) (reg counter))
    (assign product (reg t1))
    (assign counter (reg t2))
    (goto (label test-counter))
  factorial-done)

; turns out I could avoid the extra registers t1 and t2 as there's no need for them

