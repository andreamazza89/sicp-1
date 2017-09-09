(define (equ? x y) (apply-generic 'equ x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'equ? '(scheme-number scheme-number) =)
  'done)

;; (define what equality is in each package and install it, using equ? within composite types to support something like a
;; rational number whose num/den are complex
