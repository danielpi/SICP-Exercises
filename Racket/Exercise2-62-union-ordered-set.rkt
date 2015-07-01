#lang racket

; Exercise 2.62
; Give a O(n) implementation of union-set for sets represented as ordered lists.

(define (element-of-set?2 x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set?2 x (cdr set)))))

(define (intersection-set2 set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set2 (cdr set1)
                                           (cdr set2))))
              ((< x1 x2)
               (intersection-set2 (cdr set1) set2))
              ((< x2 x1)
               (intersection-set2 set1 (cdr set2)))))))

(define (adjoin-set2 x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set2 x (cdr set))))))
        
(define (union-set2 set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2)) (cons (car set1) (union-set2 (cdr set1) set2)))
        ((> (car set1) (car set2)) (cons (car set2) (union-set2 set1 (cdr set2))))
        (else (cons (car set1) (union-set2 (cdr set1) (cdr set2))))))

(union-set2 (list 1 3 4 5 6 9) (list 2 3 6 7 8 9))
(union-set2 (list 1 3 4 5 6 9) '())
(union-set2 '() (list 2 3 6 7 8 9))
