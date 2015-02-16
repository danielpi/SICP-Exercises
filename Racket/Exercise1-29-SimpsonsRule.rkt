#lang racket

; Exercise 1.29
; Simpson's Rule is a more accurate method of numerical integration than the method illustrated 
; above in section 1.3. Using Simpson's Rule, the integral of a function f between a and b is 
; approximated as ... [See pdf]

; Define a procedure that takes as arguments f, a, b and n and returns the value of the integral.
; Use your procedure to integrate cube between 0 and 1 (with n = 100 and n = 1000) and compare the
; results to those of the integral procedure shown above.

(define (cube x) (* x x x))
(define (identity x) x)
(define (inc x) (+ x 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* h k))))
  (define (simpson-term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((odd? k) 4)
             (else 2))
       (yk k)))
  (* (/ h 3) (sum simpson-term 0 inc n)))

(integral cube 0 1 100)
(integral cube 0 1 1000)