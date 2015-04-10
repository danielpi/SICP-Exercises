#lang racket

; Exercise 2.21
; The procedure square-list takes a list of numbers as argument and returns a list of the
; square of those numbers.

; (square-list (list 1 2 3 4))
; (1 4 9 16)

; Here are two different definitions of square-list. Complete both of them by filling in the
; missing expressions:

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square x) (* x x))

(define (square-list1 items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list1 (cdr items)))))

(square-list1 (list 1 2 3 4))


(define (square-list2 items)
  (map square items))

(square-list2 (list 1 2 3 4))