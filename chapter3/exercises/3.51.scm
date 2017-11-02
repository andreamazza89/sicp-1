;: (define x (stream-map show (stream-enumerate-interval 0 10)))
;: (stream-ref x 5)
;: (stream-ref x 7)

; when we start, x is a stream, with 0 as its car and a delayed stream-map of
; 1-10 in its cdr. As a result of defining x, 0 is printed.

; when evaluating (stream-ref x 5), the stream-ref procedure forces stream-map to evaluate
; the next 5 elements to reach 5. In the process, 1-2-3-4-5 are printed out

; finally, similar to above, the second call to stream-ref causes 6 and 7 to be printed out.
