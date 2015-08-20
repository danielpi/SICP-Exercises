#lang racket

; Exercise 2.76

; As a large system with generic operations evolves, new types of data
; objects or new operations may be needed. For each of the three strategies
; 1. generic operations with explicit dispatch
; 2. data directed style, and
; 3. message-passing style
; describe the changes that must be made to a system in order to add new
; types or new operations.

; For 1. If you add a new type you need a new set of constructors and you
; also need to modify all selectors so that they can handle the new type.
; To add new operations you need to write it in such a way that it handles
; all of the possible types.

; For 2. If you add a new type you need create a new set of operations and
; selectors for that type and to register those functions with the generic
; system you are using. To add a new operation requires that you go and
; write a new version of the operation for each of the available types.

; For 3. If you add a new type you need to just write the new type def along
; with all of the operations it requires. To add a new operation you need
; to go back to all of your existing types and add the new operation to
; them.



; Which organization would be most appropriate for a system in which new
; types must often be added?

; Message-passing or data directed handle new types with minimal fuss.




; Which would be most appropriate for a system in which new operations
; must often be added?

; Data-directed or explicit dispatch handle new operations with minimal
; fuss.