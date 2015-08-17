#lang racket

; Exercise 1.36
; Modify fixed-point so that it prints the sequence of approximations it generates. Then
; find a solution to x^x = 1000 by finding a fixed point of x -> log(1000)/log(x). Compare
; the number of steps this takes with and without average damping.

(define (average a b) (/ (+ a b) 2))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess)
  (newline))

(define (solv-x-power-x equals)
  (fixed-point (lambda (x) (/ (log equals) (log x))) 2.0))
(solv-x-power-x 1000)

(define (damped-solv-x-power-x equals)
  (fixed-point (lambda (x) (average x (/ (log equals) (log x)))) 2.0))
(damped-solv-x-power-x 1000)

; The damped version required far less steps than the none damped version in this case. 9 vs 35 steps.




