#lang racket

; Exercise 2.41
; Write a procedure to find all ordered triples of distinct positive integers i, j, and k less
; than or equal to a given integer n that sum to a given integer s.

; Build a list of all ordered triples that are less than n
; filter according to which triples sum to equal a value
; This should be a case where I can write a function that returns a function. I Should have a general
; function that takes two arguments, a triple and a target value, then it produces a function taht
; returns a bool if the triple sums to the target

; I need some low level functions, accumulate, flatmap and enumerate-list.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (accumulate op (op initial (car sequence)) (cdr sequence))))

(define (accumulateSICP op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap op sequence)
  (accumulate append '() (map op sequence)))

(define (enumerate-list low high)
  (if (> low high)
      '()
      (cons low (enumerate-list (+ low 1) high))))

; I need a way to generate all unique ordered triples less than n
(define (unique-triples n)
  (flatmap (lambda (i) 
             (flatmap (lambda (j) 
                        (map (lambda (k) 
                               (list i j k)) 
                             (enumerate-list 1 (- j 1)))) 
                      (enumerate-list 1 (- i 1))))
           (enumerate-list 1 n)))
(unique-triples 4)

; I need a way to calculate if a triple sums to a target value and I need that in the 
; form of a predicate that can be passed to the filter function.
(define (sum-to-target values target)
  (= (accumulate + 0 values) target))

(define (sum-to-s? s)
  (lambda (triple)
    (sum-to-target triple s)))


; Finally I can produce the final program
(define (triple-sum-combinations n s)
  (filter (sum-to-s? s) (unique-triples n)))

(triple-sum-combinations 6 9)