#lang racket

; Exercise 1.33
; You can obtain an even more general version of accumulate by introducing the notion of a 
; filter on the terms to be combined. That is, combine only those terms derived from values
; in the range that satisfy a specified condition. The resulting filtered-accumulate abstraction
; takes the same arguments as accumulate, together with an additional predicate of one argument
; that specifies the filter.

(define (filtered-accumulate predicate combiner term null-value a next b)
  (cond ((> a b) null-value)
        ((predicate a) (combiner (term a) (filtered-accumulate predicate combiner term null-value (next a) next b)))
        (else (filtered-accumulate predicate combiner term null-value (next a) next b))))

; a) Write the sum of squares of the prime numbers in the interval a to b (assuming that you have a 
; prime? predicate already written)

(define (square n) (* n n))
(define (inc n) (+ n 1))

(define (non-trivial-sqrt? n m)
  (cond ((= n 1) false)
        ((= n (- m 1)) false)
        (else (= (remainder (square n) m) 1))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (if (non-trivial-sqrt? (expmod base (/ exp 2) m) m) 
             0 
             (remainder (square (expmod base (/ exp 2) m)) m)))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (Miller-Rabin-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (cond ((< n 2) #f)
        (else (try-it (+ 1 (random (- n 1)))))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((Miller-Rabin-test n) (fast-prime? n (- times 1)))
        (else false)))
(define (prime? n)
  (fast-prime? n 100))

;(prime? 0)

(define (sum-of-squares-of-prime a b)
  (filtered-accumulate prime? + square 0 a inc b))

(sum-of-squares-of-prime 1 100)


; b) Write the product of all positive integers less than n that are relatively prime to n (i.e., all
; positive integers i < n such that GCD(i,n) == 1).

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (relative-prime? a n) (= (gcd a n) 1))
(relative-prime? 1 5)
(relative-prime? 2 5)
(relative-prime? 3 5)
(relative-prime? 4 5)

(define (product-relative-prime n)
  (define (relative-prime? a) (= (gcd a n) 1))
  
  (filtered-accumulate relative-prime? * identity 1 1 inc n))

(product-relative-prime 10)