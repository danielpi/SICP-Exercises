#lang racket

; Exercise 1.30
; a) The sum procedure is only the simplest of a vast number of similar abstractions that
; can be captured as higher-order procedures. Write an analogous procedure called product
; that returns the product of the values of a function at points over a given range.



(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (identity x) x)
(define (inc x) (+ x 1))
(product identity 1 inc 10)

; Show how to define factorial in terms of product. 

(define (factorial n)
  (product identity 1 inc n))

(factorial 5)

; Also use product to compute 
; approximations to pi using the formula

; pi   2 * 4 * 4 * 6 * 6 * 8 ...
; -- = -------------------------
;  4   3 * 3 * 5 * 5 * 7 * 7 ...

; 1 2/3
; 2 4/3
; 3 4/5
; 4 6/5
; 5 6/7
; 6 8/7
; 7 8/9

(define (pi n)
  (define (term k)
    (if (even? k)
        (/ (+ k 2) (+ k 1))
        (/ (+ k 1) (+ k 2))))   
  (* 4 (product term 1 inc n)))

(exact->inexact (pi 1000))

; b) If your product generates a recursive process, write one that generates an iterative
; proces and visa versa

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))

(define (pi-iter n)
  (define (term k)
    (if (even? k)
        (/ (+ k 2) (+ k 1))
        (/ (+ k 1) (+ k 2))))   
  (* 4 (product-iter term 1 inc n)))

(exact->inexact (pi 1000))
