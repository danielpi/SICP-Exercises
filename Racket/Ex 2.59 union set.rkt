#lang racket

; Exercise 2.59
; Implement the union-set operation for the unordered-list representation of sets.
(define (element-of-set?1 x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set?1 x (cdr set)))))

(define (adjoin-set1 x set)
  (if (element-of-set?1 x set)
      set
      (cons x set)))

(define (intersection-set1 set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set?1 (car set1) set2)
         (cons (car set1) (intersection-set1 (cdr set1) set2)))
        (else (intersection-set1 (cdr set1) set2))))

(define (union-set1 set1 set2)
  (if (null? set1)
      set2
      (union-set1 (cdr set1) (adjoin-set1 (car set1) set2)))) 

(display "union-set1")(newline)
(union-set1 (list 1 2 6 7 8 9 0)  (list 2 3 4 5 6 7))

