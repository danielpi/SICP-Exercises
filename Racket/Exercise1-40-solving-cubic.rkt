#lang racket

; Exercise 1.40
; Define a procedure cubic that can be used together with the newtons-method procedure in 
; expressions of the form (newtons-method (cubic a b c) 1) to approximate zeros of the 
; cubic x^3 + ax^2 + bx + c

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) 
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(newtons-method (cubic 5 2.5 -9) 1)
(newtons-method (cubic 5 2.5 -9) -10.0)
(newtons-method (cubic 5 2.5 -9) -2.0)
