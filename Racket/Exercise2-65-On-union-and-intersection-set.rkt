#lang racket

; Exercise 2.65
; Use the results of Exercise 2.63 and 2.64 to give O(n) implementations of union-set
; and intersection-set for sets implemented as (balanced) binary trees.

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


(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

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


(define (intersection-set set1 set2)
  (define (intersection-ordered-lists set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2) (cons x1 (intersection-ordered-lists (cdr set1)
                                           (cdr set2))))
              ((< x1 x2) (intersection-ordered-lists (cdr set1) set2))
              ((< x2 x1) (intersection-ordered-lists set1 (cdr set2)))))))
  (list->tree (intersection-ordered-lists (tree->list set1) (tree->list set2))))

(define (union-set set1 set2)
  (define (union-ordered-lists set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((< (car set1) (car set2)) (cons (car set1) (union-ordered-lists (cdr set1) set2)))
          ((> (car set1) (car set2)) (cons (car set2) (union-ordered-lists set1 (cdr set2))))
          (else (cons (car set1) (union-ordered-lists (cdr set1) (cdr set2))))))
  (list->tree (union-ordered-lists (tree->list set1) (tree->list set2))))

(define set1 (adjoin-set 10 (adjoin-set 9 (adjoin-set 5 (adjoin-set 1 (adjoin-set 4 (adjoin-set 7 '())))))))
(define set2 (adjoin-set 11 (adjoin-set 8 (adjoin-set 5 (adjoin-set 7 (adjoin-set 1 (adjoin-set 3 '())))))))
(define set3 (adjoin-set 12 (adjoin-set 7 (adjoin-set 6 (adjoin-set 1 (adjoin-set 3 (adjoin-set 5 '())))))))

(intersection-set set1 set2)
(union-set set2 set3)



