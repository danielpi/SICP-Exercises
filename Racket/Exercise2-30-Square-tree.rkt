#lang racket

; Exercise 2.30
; Define a procedure square-tree analogous to the square-list procedure of Exercise 2.21. That
; is, square-tree should behave as follows

; Define square-tree both directly (i.e., without using any higher-order procedures) and also 
; by using map and recursion.

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
; (1 (4 (9 16) 25) (36 49))


(define (square-tree2 tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree2 subtree)
             (* subtree subtree)))
       tree))

(square-tree2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
; (1 (4 (9 16) 25) (36 49))