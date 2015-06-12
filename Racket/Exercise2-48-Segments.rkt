#lang racket

; Exercise 2.48
; A directed line segment in the plane can be represented as a pair of vectors - the vector
; running from the origin to the start-point of the segment, and the vector running from 
; the origin to the end-point of the segment. Use your vector representation from Exercise
; 2.46 to define a representation for segments with a constructor make-segment and selectors
; start-segment and end-segment

(define (make-vect x y)
  (cons x y))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

; As a test make a segment that starts 1 unit above the origin and extends to the right for two units
(define seg (make-segment (make-vect 0 1) (make-vect 2 0)))
(start-segment seg)
(end-segment seg)