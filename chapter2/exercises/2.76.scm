; GENERIC OPS WITH EXPLICIT DISPATCH
; this approach is not very modular as all the operations/types must live under one name-space and for every new
; type we must update all the operations. Adding a  new operation requires it to handle all existing types.

; DATA-DIRECTED STYLE
; this is modular and means that if a new type is added, its operations just need to be registered within
; the table of operations/types. Adding an operation still requires it to be implemented for each type, but
; at least this is done within each type 'package'. It is not essential to implement the operation for each type
; but the application will assume all types to implement all operations.

; MESSAGE-PASSING STYLE
; this increases modularity even further, as it removes the need for a global table to look up operations for a
; specific type. Adding new types can be done in isolation and new operations can be added for each specific type
; though if a number of types share the same  interface, a new operation should be implemented for all types
; that respect the same interface

; I would say message-passing is best both for a system in which new types and new operation are added often,
; though I am about to look at other people's solutions and potentially change my mind....
