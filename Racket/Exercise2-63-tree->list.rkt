#lang racket

; Exercise 2.63
; Each of the following two procedures converts a binary tree to a list.

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= (entry set) x) true)
        ((> (entry set) x) (element-of-set? x (left-branch set)))
        ((< (entry set) x) (element-of-set? x (right-branch set)))))


(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= (entry set) x) set)
        ((> (entry set) x) (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set)))
        ((< (entry set) x) (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set))))))


(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

; 1 cons per entry
; 1 append for every left branch

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

; a. Do the two procedures produce the same result for every tree? If not, how do the
;    results differ? What lists do the two procedures produce for the trees in Figure
;    2.16

(define set1 (make-tree 7 (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '())) (make-tree 9 '() (make-tree 11 '() '()))))
(define set1-2 (adjoin-set 11 (adjoin-set 9 (adjoin-set 5 (adjoin-set 1 (adjoin-set 3 (adjoin-set 7 '())))))))
set1-2
(define set2 (adjoin-set 11 (adjoin-set 9 (adjoin-set 5 (adjoin-set 7 (adjoin-set 1 (adjoin-set 3 '())))))))
set2
(define set3 (adjoin-set 11 (adjoin-set 7 (adjoin-set 9 (adjoin-set 1 (adjoin-set 3 (adjoin-set 5 '())))))))
set3

(display "Tree 1")(newline)
set1
(display "tree->list-1: ") (tree->list-1 set1)
(display "I thought   : ") (list 7 9 11 3 5 1)
(display "tree->list-2: ") (tree->list-2 set1)
(display "I thought   : ") (list 1 3 5 7 9 11)

(display "Tree 2")(newline)
set2
(display "tree->list-1: ") (tree->list-1 set2)
(display "I thought   : ") (list 1 3 5 7 9 11)
(display "tree->list-2: ") (tree->list-2 set2)
(display "I thought   : ") (list 1 3 5 7 9 11)

(display "Tree 3")(newline)
set3
(display "tree->list-1: ") (tree->list-1 set3)
(display "I thought   : ") (list 1 3 5 7 9 11)
(display "tree->list-2: ") (tree->list-2 set3)
(display "I thought   : ") (list 1 3 5 7 9 11)

; So both functions provide the same lists for all three of the trees.

; b. Do the two procedures have the same order of growth in the number of steps 
;    required to convert a balanced tree with n elements to a list? If not, which
;    one grows more slowly?

; I don't get this. Apartently append has a constant time factor so the first function
; runs in O(n log n). The second doesn't use append so it runs in O(n).