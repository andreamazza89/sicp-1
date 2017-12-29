; a
(controller
    (assign continue (label after-expt))
  expt-loop
    (test (op =) (reg n) (const 0))
    (branch (label base-case))
    (save continue)
    (save b)
    (assign continue (label after-loop))
    (assign n (op -) (reg n) (const 1))
    (goto (label expt-loop))
  after-loop
    (restore b)
    (restore continue)
    (assign val (op *) (reg val) (reg b))
    (goto (reg continue))
  base-case
    (assign val (const 1))
    (goto (reg continue))
  expt-done)

; b

(controller
    (assign product (const 1)
  expt-iter
    (test (op =) (reg n) (const 0))
    (branch expt-done) ; product has the result in it
    (assign n (op -) (reg n) (const 1))
    (assign product (op *) (reg product) (reg b))
    (goto (label expt-iter))
  expt-done)
