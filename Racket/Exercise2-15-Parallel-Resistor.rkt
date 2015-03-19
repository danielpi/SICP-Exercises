#lang racket

; Exercise 2.15
; Another user has also noticed the different intervals computed by different but algebraically
; equivalent expressions. She says that a formula to compute with intervals like ours will produce
; tighter error bounds if it can be written in such a form that no variable that represents an 
; uncertain number is repeated. Thus par2 is a "better" program for parallel resistances.
; Is this right?


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

; Certainly par2 gives tighter error bounds so this statement is right.