#lang racket

; Exercise 2.46
; A two-dimensional vector v running from the origin to a point can be represented as a pair
; consisting of an x-coordinate and a y-coordinate. Implement a data abstraction for vectors
; by giving a constructor make-vect and corresponding selectors xcor-vect and ycor-vect. In 
; terms of your selectors and constructor, implement procedures add-vect, sub-vect, and 
; scale-vect that perform the operations vector additon, vector subtraction, and multiplying 
; a vector by a scalar:

; (x1,y1) + (x2,y2) = (x1 + x2, y1 + y2)
; (x1,y1) - (x2,y2) = (x1 - x2, y1 - y2)
;         s * (x,y) = (sx, sy)


(define (make-vect x y)
  (cons x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))


(define (add-vect vec1 vec2)
  (make-vect (+ (xcor-vect vec1) (xcor-vect vec2))
             (+ (ycor-vect vec1) (ycor-vect vec2))))

(define (sub-vect vec1 vec2)
  (make-vect (- (xcor-vect vec2) (xcor-vect vec1))
             (- (ycor-vect vec2) (ycor-vect vec1))))

(define (scale-vect scale vect)
  (make-vect (* scale (xcor-vect vect))
             (* scale (ycor-vect vect))))

(define a (make-vect 3 4))
(define b (make-vect 2 1))
(xcor-vect a)
(ycor-vect b)
(add-vect a b)
(sub-vect b a)
(scale-vect 0.5 a)