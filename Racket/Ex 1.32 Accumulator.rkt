#lang racket

; Exercise 1.32
; a) Show that sum and product are both special cases of a still more general notion
; called accumulate that combines a collection of terms using some general accumulation
; function

; (accumulate combiner null-value term a next b)

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

; Testing the above out
(define (identity x) x)
(define (inc x) (+ x 1))
(define (cube x) (* x x x))


(define (factorial n)
  (product identity 1 inc n))

(factorial 5)


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


; b) Write an iterative version of accumulate

(define (accumulate-iter combiner null-value term a next b)
  (define (iter combiner a result)
    (if (> a b)
        result
        (iter combiner (next a) (combiner (term a) result))))
  (iter combiner a null-value))

(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))


(define (factorial-iter n)
  (product-iter identity 1 inc n))

(factorial-iter 5)


(define (integral-iter f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* h k))))
  (define (simpson-term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((odd? k) 4)
             (else 2))
       (yk k)))
  (* (/ h 3) (sum-iter simpson-term 0 inc n)))

(integral-iter cube 0 1 100)
(integral-iter cube 0 1 1000)
