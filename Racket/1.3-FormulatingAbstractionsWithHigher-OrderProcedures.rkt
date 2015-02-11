#lang racket

; 1.3 Formulating Abstractions with Higher-Order Procedures
; Procedures are abstractions that describe compound operations on numbers
; independent of the particular numbers. For example

(define (cube x) (* x x x))

; Above is a method for obtaining a cube of any number. We don't need it

(* 3 3 3)

; This would limit us to a languages primitive procedures. We wold be unable to
; express the concept of cubing.

; To go a step beyond this we also need the ability construct procedures that
; can accept procedures as arguments or return procedures as values. These are
; called higher-order procedures.

; 1.3.1 Procedures as Arguments
; Consider the following three procedures

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))
(sum-integers 1 10)

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))
(sum-cubes 1 10)

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum(+ a 4) b))))
(pi-sum 1 10000)
; Approaches pi/8 slowly 0.39269908169872
(* (pi-sum 1 10000) 8)






