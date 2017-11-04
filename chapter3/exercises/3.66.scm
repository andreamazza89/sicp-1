; let's call the first two streams A and B
; the first pair is (A0,B0)
; the following pairs are the result of interleaving C | A0,B1 A0,B2 A0,B3 ...
; and D | A1,B1 A1,B2 ..

; A0,B0 A0,B1 A0,B2 A0,B3 A0,B4 A0,B5 (the cdr of this one is C)
;       A1,B1 A1,B2 A1,B3 A1,B4 A1,B5 (everything under the first line is D)
;             A2,B2 A2,B3 A2,B4 A2,B5
;                   A3,B3 A3,B4 A3,B5

; looking at the shallowest level, we are going to see a pair from the top row
; every other pair, with the frequency decreasing the further we move into the stream
; as there are more and more pairs.

; how many pairs precede (1,100)? we are on the first row, so it should be around 199 I think.
; how about (99,100)? this is now on the 99th row, where things go pretty slow. struggling to
; figure out exactly how many precede it, gonna say a ton.

; ditto for (100,100), probably a ton and a ton more..
