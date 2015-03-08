#lang racket


; Exercise 2.3
; Implement a representation for rectangles in a plane. In terms of your constructors and selectors,
; create procedures that compute the perimeter and the area of a given rectangle. Now implement a 
; different representation for rectangles. Can you design your system with suitable abstractions
; barriers, so that the same perimeter and area procedures will work using either representation?

; from Exercise 2.2
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

;       ------------------------------------
;  ----- Programs that use rational numbers -----
;       ------------------------------------
;        Rational numbers in problem domain
;
;               ---------------------
;  -------------   perimeter area    ------------
;               ---------------------
;  Rectangles as perimeter and area
;
;               ----------------------
;  ------------- make-rect  -----------
;               ----------------------
;           Rectangles as origin point, height and a width
;
;                   --------------
;  ----------------- cons car cdr ----------------
;                   --------------
;         However pairs are implemented

; A point is a x,y coordinate pair
; A line is two points
; A rectangle could be an origin and a size (width and height)
; Or a rectangle could be an origin and a diagonal point

; a) 
(define (make-size w h)
  (cons w h))
(define (size-width x)
  (car x))
(define (size-height x)
  (cdr x))

(define (make-rect origin size)
  (cons origin size))
(define (rect-origin x)
  (car x))
(define (rect-size x)
  (cdr x))

(define (perimeter rect)
  (let ((width (size-width (rect-size rect)))
        (height (size-height (rect-size rect))))
    (+ width width height height)))
(define (area rect)
  (let ((width (size-width (rect-size rect)))
        (height (size-height (rect-size rect))))
    (* width height)))

(define a-rect (make-rect (make-point 3 5) (make-size 2 2)))
(perimeter a-rect)
(area a-rect)

; b)
(define (make-rect2 p1 p2)
  (cons p1 p2))
(define (rect-p1 x)
  (car x))
(define (rect-p2 x)
  (cdr x))
(define (rect-size2 x)
  (make-size (abs (- (x-point (rect-p1 x))
                     (x-point (rect-p2 x))))
             (abs (- (y-point (rect-p1 x))
                     (y-point (rect-p2 x))))))

(define (perimeter2 rect)
  (let ((width (size-width (rect-size2 rect)))
        (height (size-height (rect-size2 rect))))
    (+ width width height height)))
(define (area2 rect)
  (let ((width (size-width (rect-size2 rect)))
        (height (size-height (rect-size2 rect))))
    (* width height)))


(define b-rect (make-rect2 (make-point 3 5) (make-point 5 7)))
(perimeter2 b-rect)
(area2 b-rect)