#lang racket

; Excercise 2.6
; In case representing pairs as procedures wasn't mind-boggling enough, consider that, in a language
; that can manipulate procedures, we can get by without numbers (at least insofar as nonnegative 
; integers are concerned) by implementing 0 and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; This representation is known as Church numerals, after its inventor, Alonzo Church, the logician
; who invented the Lambda calculus.

; Define one and two directly (not in terms of zero and add-1) (Hint: Use substitution to evaluate
; (add-1 zero)).

(add-1 zero)

; Substitution of above
; (add-1 zero)
; - Grab the body of add-1
; (lambda (f) (lambda (x) (f ((n f) x))))
; - Replace the formal parameters n with the body of zero
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))

; I do not get this at all

; Church numerals encode numbers by the number of times f is called on x. So
; 0 = x
; 1 = f(x)
; 2 = f(f(x)
; etc

(define zero2 (lambda (f) (lambda (x) x)))
(define one (lambda (f) (f (lambda (x) x))))
(define two (lambda (f) (f (f (lambda (x) x)))))

; Give a direct definition of the addition procedure + (not in terms of repeated application of
; add-1).

(define (church+ a b)
  