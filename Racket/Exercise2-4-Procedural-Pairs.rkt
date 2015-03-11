#lang racket

; Exercise 2.4
; Here is an alternative procedural representation of pairs. For this representation, verify that
; (car (cons x y)) yields x for any objects x and y.

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

; What is the corresponding definition of cdr? (Hint: To verify that this works, make use of the
; substitution model of section 1.1.5)

(define z (cons 1 2))
(car z)

; Substitution model of what happened above
; (cons 1 2)
; - Grab the body of cons
; (lambda (m) (m x y))
; - Replace the formal parameters x y with 1 2
; (lambda (m) (m 1 2))
; So (cons 1 2) returns the function (lambda (m) (m 1 2))

; Now to see how car works
; (car z)
; - Grab the body of car
; (z (lambda (p q) p))
; - Replace the formal parameters z with the function (lambda (m) (m 1 2))
; ((lambda (m) (m 1 2)) (lambda (p q) p))
; - Now substituting (lambda (p q) p) in as the value of m
; ((lambda (p q) p) 1 2)
; - This is a function that takes two arguments and returns the firs
; 1

(define (cdr z)
  (z (lambda (p q) q)))
(cdr z)


