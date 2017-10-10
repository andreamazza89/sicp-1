; this creates an infinitely recursive data structure:

;  a|-> b|-> c|-
;  ^-----------|

; hence trying to compute the last pair of such data structure creates an infinite loop
