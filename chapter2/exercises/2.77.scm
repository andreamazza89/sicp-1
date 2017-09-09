; As described in the exercise, the complex number accessors are only available within the complex numbers package.
; In other words, you can only ask for the magnitude of a number when you know that the number is complex.
; A generic 'scheme-number can be complex, rational, integer, or anything else that supports add/subtract/multiply/divide.

; What the exercise asks seems to break encapsulation, and means adds complex-number-specific operations to the same
; interface as the other numbers.

; The solution presented exposes the selector procedures at a higher level, making it possible to access internal details
; of a complex number.

; Now that the magnitude procedure is available at high level, applying it to a 'scheme-number (needs to be be a complex one)
; calls apply-generic (x1) with complex->rectangular->3,4 to retrieve the magnitude procedure.
; The magnitude procedure then also calls apply-generic (x2) to unpack the complex type from rectangular->3,4, to finally
; return with the value of applying magnitude (the rectangular version of it) to 3,4
