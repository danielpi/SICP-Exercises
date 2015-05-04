#lang racket

; Exercise 2.34
; Evaluating a polynomial in x at a given value of x can be formulated as an accumulation. We
; evaluate the polynomial

; anx^n + an-1x^n + ... + a1x + a0

; using a well-known algorithm called Horner's rule, which structures computation as

; (...(anx + an-1)x + ... + a1)x + a0

; In other words, we start with an, multiply by x, add an-1, multiply by x, and so on, until we 
; reach a0.

; Fill in the following template to produce a procedure that evaluates a polynomial using
; Horner's rule. Assume that the coefficients of the polynomial are arranged in a sequence,
; from a0 through an.

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

; For example, to compute 1 + 3x + 5x^3 + x^5 at x = 2 you would evaluate (answer should be 79)
(horner-eval 2 (list 1 3 0 5 0 1))