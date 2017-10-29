; 100*100*100 -> P1 sets x, then P2 sets x
; 1000*1000 -> P2 sets x, then P1 sets x
; 10*1000 -> P2 sets x between the two reads of P1
; 10*100*100 -> P1 sets x between first and second read of P2
; 10*10*100 -> P1 sets x between second and third read of P2

; after serialisation, only the top two resuls are possible.
