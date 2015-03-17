#lang racket

; Exercise 2.13
; Show that under the assumption of small percentage tolerances there is a simple formula for the
; approximate percentgae tolerance of the product of two intervals in terms or the tolerances of
; the factors. You may simplify the problem by assuming that all numbers are positive.


(define (make-interval lower upper)
  (cons lower upper))
(define (lower-bound interval)
  (car interval))
(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
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


(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


(define (make-center-percent c t)
  (make-interval (- c (* c t)) (+ c (* c t))))
(define (percent i)
  (/ (width i) (center i)))

(define a (make-center-percent 10 0.1))
(define b (make-center-percent 5 0.02))
(define Axb (mul-interval a b))
Axb
(center Axb)
(percent Axb)

; For small tolerances the tolerance of an interval made by multiplying two other intervals is
; roughly equal to the two tolerances added together.
