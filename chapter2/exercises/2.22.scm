; The reason why the list returned from the first procedure is in reverse order is because this works across the list left
; to right, adding each element to the beginning of the new list.

; The second procedure does not work because it is consing a list to a number. Cons should be given a number as the first
; input, and the list to cons it to as the second element.
