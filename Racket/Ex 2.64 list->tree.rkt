#lang racket

; Exercise 2.64
; The following procedure list->tree converts an ordered list to a balanced binary tree. The
; helper procedure partial-tree takes as arguments an integer n and a list of at least n elements
; and constructs a balanced tree containing the first n elements of the list. The result returned
; by partial-tree is a pair (formed with cons) whose car is the constructed tree and whose cdr
; is the list of elements not included in the tree.

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

; a. Write a short paragraph explaining as clearly as you can how partial-tree
;    works. Draw the tree produced by list->tree for the list (1 3 5 7 9 11)

; First up we calculate how many entries we would like on the left branch (half). We then
; recursively ask for the left half of the items to be turned into a partial-tree. Then
; we take that result and strip out the tree, the remaining elements and we calculate the
; number of elements in the right side of the tree. Now grab the value that will be used
; for this-entry and recursively ask for the right side partial-tree. Split off the
; right tree and the remaining elements. Make a tree with this-entry, the left-tree and
; right-tree.

;      5
;    /   \
;   1     9
;    \   / \
;     3 7   11


(list->tree (list 1 3 5 7 9 11))

; b. What is the order of growth in the number of steps required by list->tree to
;    convert a list of n elements?

; At each step partial-tree splits a list of length n into two lists of approximate length
; n / 2. The work done to split the list is (quotient (- n 1) 2) and (- n (+ left-size 1)), 
; both of which take constant time. The work to combine the results is 
; (make-tree this-entry left-tree right-tree), and is also constant time. Therefore, the
; time to make the partial tree of a list of n elements is
; T(n) = 2T(n/2) + O(1)
; By the mater theorem??? T(n) = O(n)


