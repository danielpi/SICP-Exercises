#lang racket

; Exercise 1.44
; The idea of smoothing a function is an important concept in signal processing. If f is a function
; and dx is some small number, then the smoothed version of f is the function whose value at a 
; point x is the verage of f(x - dx), f(x), and f(x + dx).

; Write a procedure smooth that takes as input a procedure that computes f and returns a procedure
; that computes the smoothed f.

(define dx 0.001)
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) 
                    (f x)
                    (f (+ x dx))) 
                 3)))

(define (sawtooth-wave x)
  (- (* 2 (- x (floor x))) 1))

(define (smoothed-saw x)
  ((smooth sawtooth-wave) x))

(define (apply-linspace f start finish steps)
  (newline)
  (define (iter step)
    (newline)
    (display (exact->inexact (f (+ start (* (/ (- finish start) steps) step)))))
    (f (+ start (* (- finish start) step)))
    (if (= step steps)
        (newline)
        (iter (+ step 1))))
  (iter 0))

(display "Sin Wave")
(apply-linspace sin 0 3 50)
(display "Smoothed Sin Wave")
(apply-linspace (smooth sin) 0 3 50)


; It is sometimes valuable to repeatedly smooth a function (that is smooth the smoothed function and so on)
; to obtain the n-fold smoothed function. Show how to generate the n-fold smoothed function of any given
; function using smooth and repeated from exercise 1.43

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter g step)
    (if (= step 1)
        g
        (iter (compose f g) (- step 1))))
  (iter f n))
 
((repeated smooth 5) sin)


