#lang racket

; Exercise 2.2
; Consider the problem of representing line segments in a plane. Each segment is
; represented as a pair of points: a starting point and an ending point. Define a 
; constructor make-segment and selectors start-segment and end-segment that define
; the representation of segments in terms of points. 

; Furthermore, a point can be represented as a pair of numbers: the x coordinate
; and the y coordinate. Accordingly, specify a constructor make-point and selectors
; x-point and y-point that define this representation.

; Finally, using your selectors and constructors, define a procedure midpoint-segment 
; that takes a line segment as argument and returns its midpoint (the point whose 
; coordinates are the average of the coordinates of the endpoints).

; To try your procedures, you'll need a way to print points:

(define (make-point x y)
  (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point (make-point 3 5))

(define (make-segment start end)
  (cons start end))
(define (start-segment l) (car l))
(define (end-segment l) (cdr l))

(define (average a b) (/ (+ a b) 2))

(define (midpoint-segment l) 
  (make-point (average (x-point (start-segment l))
                       (x-point (end-segment l)))
              (average (y-point (start-segment l))
                       (y-point (end-segment l)))))

(define origin (make-point 0 0))
(define p1 (make-point 3 3))
(define line (make-segment origin p1))
(print-point (midpoint-segment line))