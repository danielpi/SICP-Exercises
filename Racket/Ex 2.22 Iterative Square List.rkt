#lang racket

; Exercise 2.22
; Louis Reasoner tries to rewrite the first square-list procedure of exercise 2.21 so that it
; evolves an iterative process:

(define (square x) x * x)

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))
(square-list (list 1 2 3 4))

; Unfortunately, defining square-list this way produces the answer list in the reverse order
; of the one desired. Why?

; Because the algorithm builds itself up and then processes through the list in reverse order.

; Louis then tries to fix his bug by interchanging the arguments to cons

(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))
(square-list2 (list 1 2 3 4))

; This doesn't work either. Explain.

; This gets the order right but the result isn't a proper list. A list is a series of pairs with
; the left item being a value and the right item being a pointer to the next pair. Here the
; order has been swapped with the left value being a pointer to the previous pair and the
; right being the value.