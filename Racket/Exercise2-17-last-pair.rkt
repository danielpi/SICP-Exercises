#lang racket

; Exercise 2.17
; Define a procedure last-pair that returns the list that contains only the 
; last element of a given (nonempty) list:

(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))

(last-pair (list 23 72 149 34))