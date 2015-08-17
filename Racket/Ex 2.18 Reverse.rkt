#lang racket

; Exercise 2.18
; Define a procedure reverse that takes a list as argument and returns a list 
; of the same elements in reverse order:

(define (reverse list)
  (define (reverse-iter list reverse-list)
    (if (null? list)
        reverse-list
        (reverse-iter (cdr list) (cons (car list) reverse-list))))
  (reverse-iter list '()))


(reverse (list 1 4 9 16 25))