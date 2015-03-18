#lang racket

; Exercise 2.14


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

; Demonstrate that par1 and par2 give different rsults for the same input.

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
(define r1 (make-center-percent 1000 0.01))
(define r2 (make-center-percent 3300 0.05))
(par1 r1 r2)
(par2 r1 r2)

; Investigate the behaviour of the system on a variety of arithmetic expressions.
(define one (make-interval 1 1))
(div-interval r1 r1)
(div-interval r1 one)
(mul-interval one r1)
(add-interval one r1)

; Make some intervals A and B and use them in computing the expressions A/A and A/B. You
; will get the most insight by using intervals whose width is a small precentage of the 
; center value.

; Examine the results of the computation in center-percent form


