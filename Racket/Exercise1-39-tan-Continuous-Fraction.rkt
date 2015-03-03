#lang racket

; Exercise 1.39
; A continued fraction representation of the tangent function was published in 1770 by
; the German mathematician J.H. Lambert.
;
;               x
; tan x = ---------------
;         1 -     x^2
;             -----------
;             3 -   x^2
;                 -------
;                 5 - ...
;
; where x is in radians. Define a procedure (tan-cf x k) that computes an approximation to the
; tangent function based on Lambert's formula.

(define (cont-frac Ni Di k)
  (define (recur i)
    (if (> i k)
        0
        (/ (Ni i) (+ (Di i) (recur (+ i 1))))))
  (recur 1))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        (* -1 x)
        (* -1 x x)))
  (define (d i)
    (- (* i 2) 1))
  (* -1 (cont-frac n d k)))

(tan-cf 1.0 10)