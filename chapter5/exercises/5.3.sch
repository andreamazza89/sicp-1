; using good-enough? and improve as primitives
(controller
  test-good-enough
    (test (op good-enough?) (reg guess) (reg x))
    (branch (label sqrt-done))
    (assign guess (op improve) (reg guess) (reg x))
    (goto (label test-good-enough))
  sqrt-done)

; implementing good-enough?
(controller
  sqrt-iter
    (assign squared-guess (op *) (reg guess) (reg guess))
    (test (op >) (reg squared-guess) (reg x))
    (branch (label guess-is-larger))
    (goto guess-is-smaller)
  guess-is-larger
    (assign sqrt-difference (op -) (reg squared-guess) (reg x))
  guess-is-smaller
    (assign sqrt-difference (op -) (reg x) (reg squared-guess))
  test-good-enough
    (test (op <) (reg sqrt-difference) (const 0.0001))
    (branch (label sqrt-iter-done))
    (assign guess (op improve) (reg guess) (reg x))
    (goto (label sqrt-iter))
  sqrt-iter-done)
