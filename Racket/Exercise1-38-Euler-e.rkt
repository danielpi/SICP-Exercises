#lang racket

; Exercise 1.38
; In 1737 the Swiss mathematician Leonhard Euler published a continued fraction expansion
; for e - 2, where e is the base of the natural logarithms. In this fraction Ni are all 1
; and the Di are successively 1 2 1 1 4 1 1 6 1 1 8 etc. Write a program that uses cont-frac
; to approximate e


; 1 2 3 4 5 6 7 8 9 10 11
; 1 2 0 1 2 0 1 2 0
; 0 0 1 1 1 2 2 2 3 3
; 1 2 1 1 4 1 1 6 1 1  8

(define (cont-frac Ni Di k)
  (define (recur i)
    (if (> i k)
        0
        (/ (Ni i) (+ (Di i) (recur (+ i 1))))))
  (recur 1))

(define (euler)
  (define (euler-next i)
    (if (= 0 (remainder (+ i 1) 3)) 
        (* 2 (quotient (+ i 1) 3))
        1))
  (+ 2 (cont-frac (lambda (i) 1.0)
             euler-next
             100)))

(euler)
