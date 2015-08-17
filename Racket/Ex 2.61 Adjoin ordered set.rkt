#lang racket

; Exercise 2.61
; Give an implementation of adjoin-set using the ordered representation. By analogy with 
; element-of-set? show how to take advantage of the ordering to produce a procedure that
; requires on the average about half as many steps as with the unordered representation.

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
        

(adjoin-set2 1 (list 1 3 7 8 9)) ; This will run a lot quicker than
(adjoin-set2 9 (list 1 3 7 8 9)) ; this
(adjoin-set2 2 (list 1 3 7 8 9))  ; Likewise this will run quicker
(adjoin-set2 11 (list 1 3 7 8 9)) ; than this will