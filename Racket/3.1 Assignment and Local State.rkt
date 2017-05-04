#lang racket

; 3.1 Assignment and Local State

; We ordinarily view the world as populated by independent objects, each of which has
; a state that changes over time. An object is said to "have state" if its behavior is
; influenced by its history. A bank account, for example, has state in that the answer
; to the question "Can I withdraw $100?" depends upon the history of deposit and
; withdrawal transactions. We can characterize an object's state by one or more state
; variables, which among them maintain enough information about history to determine
; the object's current behavior. In a simple banking system, we could characterize the
; state of an account by an account by a current balance rather than by remembering
; the entire history of account transactions.

; 