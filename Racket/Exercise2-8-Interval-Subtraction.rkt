#lang racket

; Exercise 2.8
; Describe how the difference of two intervals may be computed. Define a corresponding subtraction
; procedure, called sub-interval

; Subtract the two lower bounds and subtract the two upper bounds.

(define (make-interval lower upper)
  (cons lower upper))
(define (lower-bound interval)
  (min (car interval) (cdr interval)))
(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define a (make-interval 9 11.1))
(define b (make-interval 4.5 5.5))

(define c (sub-interval a b))
(lower-bound c)
(upper-bound c)
