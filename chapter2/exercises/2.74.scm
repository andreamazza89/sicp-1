(define (get-record file employees-name)
  ((get 'get-record (division-id file)) employees-name (division-file file)))

; each division should provide a pair with the division-id and file, and install the get-record procedure using their dividion-id
; so it can be looked up

(define (get-salary employee-record)
  ((get 'get-salary (division-id employee-record) (employee-data employee-record))))

; not sure c) is well worded. It seems to describe the same requirements as a), so I either I have misunderstood a) or it must
; mean find an employee's record across all of the divisions, rather than within one division.


; d) no changes at all, as long as the new company respects the contract
