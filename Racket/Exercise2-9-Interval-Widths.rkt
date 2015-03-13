#lang racket

; Exercise 2.9
; The width of an interval is half of the difference between its upper and lower bounds. The width is a measure
; of the uncertainty of the number specified by the interval. For some arithmetic operations the width of
; the result of combining two intervals is a function only of the widths of the argument intervals, whereas for
; others the width of the combination is not a function of the widths of the argument intervals. 

; Show that the width of the sum (or difference) of two intervals is a function only of the widths of the 
; intervals being added (or subtracted). 

; Give examples to show that this is not true for multiplication or division.

(define (make-interval lower upper)
  (cons lower upper))
(define (lower-bound interval)
  (min (car interval) (cdr interval)))
(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

; For addition and subtraction the widths of the two arguments are either added or subtracted from each
; other.
(define (width interval)
  (- (upper-bound interval) (lower-bound interval)))
(define (width-of-added a b)
  (+ (width a) (width b)))
(define (width-of-sub a b)
  (- (width a) (width b)))

(define r1 (make-interval 0.9 1.1))
(define r2 (make-interval 9 11))

(width-of-added r1 r2) ; Should be 2.2
(width-of-sub r2 r1) ; Should be 1.8

; For multiplication it is not so easy
(define r1r2 (mul-interval r1 r2))
(lower-bound r1r2)
(upper-bound r1r2)
(width r1r2)

(define r1r1 (mul-interval r1 r1))
(lower-bound r1r1)
(upper-bound r1r1)
(width r1r1)

(define r2r2 (mul-interval r2 r2))
(lower-bound r2r2)
(upper-bound r2r2)
(width r2r2)