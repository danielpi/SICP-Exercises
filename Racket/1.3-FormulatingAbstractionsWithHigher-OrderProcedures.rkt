#lang racket

; 1.3 Formulating Abstractions with Higher-Order Procedures
; Procedures are abstractions that describe compound operations on numbers
; independent of the particular numbers. For example

(define (cube x) (* x x x))

; Above is a method for obtaining a cube of any number. We don't need it

(* 3 3 3)

; This would limit us to a languages primitive procedures. We wold be unable to
; express the concept of cubing.

; To go a step beyond this we also need the ability construct procedures that
; can accept procedures as arguments or return procedures as values. These are
; called higher-order procedures.

; 1.3.1 Procedures as Arguments
; Consider the following three procedures

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))
(sum-integers 1 10)

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))
(sum-cubes 1 10)

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum(+ a 4) b))))
(pi-sum 1 10000)
; Approaches pi/8 slowly 0.39269908169872
(* (pi-sum 1 10000) 8)

; These three procedures share a common underlying pattern. They are identical except for
; - name of the procedure
; - the function of a used to compute the term to be added
; - the function that provides the next value of a
;
; Template
; (define (<name> a b)
;   (if (>a b)
;       0
;       ( + (<term> a)
;           (<name> (<next> a) b))))
;
; This is very similar to what mathematicians refer to as a summation of a series,
; "sigma notation". This allows mathematicians to deal with the concept of summation
; itself rather than only with particular sums. 
;
; We can do this by transforming the "slots" from above into formal parameters.

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; Notice that sum takes as its arguments the lower and upper bounds a and b as well 
; as the procedures term and next.

(define (inc n) (+ n 1))
(define (sum-cubes2 a b)
  (sum cube a inc b))

(sum-cubes2 1 10)


(define (identity x) x)
(define (sum-integers2 a b)
  (sum identity a inc b))

(sum-integers2 1 10)


(define (pi-sum2 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum2 1 1000))


; Once we have sum we can use it as a building block in formulating further concepts. For
; instance, the definite integral of a function f between the limits a and b can be
; approximated numerically using the formula ...
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
(integral identity 0 1 0.01)
(integral identity 0 1 0.001)




