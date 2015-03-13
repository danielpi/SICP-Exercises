#lang racket

; Exercise 2.7
; Implement the interval abstraction.

(define (make-interval lower upper)
  (cons lower upper))
(define (lower-bound interval)
  (car interval))
(define (upper-bound interval)
  (cdr interval))

(define a (make-interval 9 1.1))
(lower-bound a)
(upper-bound a)
