#lang racket

; Exercise 2.10
; An expert systems programmer, looks over your shoulder and comments that it is not 
; clear what it means to divide by an interval that spans zero. Modify your code to 
; check for this condition and to signal an error if it occurs.


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
(define (spans-zero interval)
  (if (> (upper-bound interval) 0)
      (< (lower-bound interval) 0)
      #f))
(define (div-interval x y)
  (if (or (spans-zero x) (spans-zero y))
      (error "Interval division spans zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define r1 (make-interval -1 3))
(define r2 (make-interval 2 5))
(define r3 (make-interval -4 -2))
(define r4 (make-interval 0 1))
(define r5 (make-interval -4 0))

(div-interval r2 r3)
;(div-interval r1 r2) ; Interval division spans zero
;(div-interval r3 r4) ; division by zero
;(div-interval r3 r1) ;  Interval division spans zero
