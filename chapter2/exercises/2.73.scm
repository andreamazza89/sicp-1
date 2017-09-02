;a) Type differentiation is based on the expression's operation. This is established by looking at the first symbol in the
;   operation (i.e. '+, '-, '*, etc.), which acts as a type tag. Since numbers and variables are a single (variable) symbol,
;   these cannot be used as a type switch. One possible solution would be to change the representation of numbers and variables
;   as '(number 3) and '(variable a) respectively; this would allow type dispatch I think.

;b)
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (install-deriv-package)

;; internal procedures
(define (deriv-for-sum operands var)
  (make-sum (deriv (addend operands) var)
            (deriv (augend operands) var)))

;; interface to the rest of the system
(put 'deriv '(+) deriv-for-sum)
