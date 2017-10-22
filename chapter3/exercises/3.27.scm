; no drawing, sozzy.

; for each step of the calculation, the n-2 calculation will already be available due to memoization
; so the number of steps is proportional to n, no matter how large n is.

; because the function fib is recursive, simply memoizing the entry point is not as advantageous as also
; memoizing the recursive calls
