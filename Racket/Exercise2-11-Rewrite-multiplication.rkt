#lang racket

; Exercise 2.10
; By testing the signs of the endpoints of the intervals it is possible to break 
; mul-interval into nine cases, only one of which requires more than two multiplications.

; Rewrite this procedure to take advantage of this fact.


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
(define (mul-interval-old x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
;(define (mul-interval x y)
;  (cond 
(define (spans-zero interval)
  (if (> (upper-bound interval) 0)
      (< (lower-bound interval) 0)
      #f))
(define (div-interval x y)
  (if (or (spans-zero x) (spans-zero y))
      (error "Interval division spans zero")
      (mul-interval-old x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define r1 (make-interval -1 3))
(define r2 (make-interval 2 5))
(define r3 (make-interval -4 -2))
(define r4 (make-interval 0 1))
(define r5 (make-interval -4 0))

(mul-interval-old r1 r1)
(mul-interval-old r2 r2)
(mul-interval-old r3 r3)