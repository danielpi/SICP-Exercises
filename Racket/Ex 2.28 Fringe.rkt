#lang racket

; Exercise 2.28
; Write a procedure fringe that taeks as argument a tree (represented as a list) and returns a list
; whose elements are all the leaves of the tree arranged in left-to-right order. For example,

(define x (list (list 1 2) (list 3 4)))
x ; '((1 2) (3 4))
; (fringe x)
; (1 2 3 4)
; (fringe (list x x))
; (1 2 3 4 1 2 3 4)

(define (fringe tree)
  (if (null? tree)
      '()
       (if (pair? (car tree))
           (append (fringe (car tree)) (fringe (cdr tree)))
           (cons (car tree) (fringe (cdr tree))))))
            
(fringe x)

; For our example this is what the above code reduces to.
(append (cons 1 (cons 2 '())) (cons 3 (cons 4 '())))