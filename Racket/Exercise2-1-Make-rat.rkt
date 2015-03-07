#lang racket

; Exercise 2.1
; Define a better version of make-rat that handles both positive and negative arguments.
; Make-rat should normalize the sign so that if the rational number is positive, both the
; numerator and denominator are positive, and if the rational number is negative, only
; the numerator is negative.

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (gcd a b)
  (if (= b 0)
      (abs a)
      (gcd b (remainder a b))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (isPositive? x) (> x 0))
(define (isNegative? x) (< x 0))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (isNegative? d)
        (cons (* -1 (/ n g)) (* -1 (/ d g)))
        (cons (/ n g) (/ d g)))))

(print-rat (make-rat -1 -2)) ;  1/2
(print-rat (make-rat 1 -2))  ; -1/2
(print-rat (make-rat 1 2))   ;  1/2
(print-rat (make-rat -1 2))  ; -1/2

