#lang racket

; Exercise 2.5
; Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations
; if we represent the pair a and b as the integer that is the product 2^a * 3^b. Give the corresponding
; definitions fo the procedures cons, car and cdr.

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(cons 1 1)

(define (car z)
  (define (iter total count)
    (if (= 1 (remainder total 2))
        count
        (iter (/ total 2) (+ count 1))))
  (iter z 0))
(car (cons 10 5))

(define (cdr z)
  (define (iter total count)
    (if (not (= 0 (remainder total 3)))
        count
        (iter (/ total 3) (+ count 1))))
  (iter z 0))
(cdr (cons 10 12))