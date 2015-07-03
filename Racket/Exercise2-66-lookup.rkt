#lang racket

; Exercise 2.66
; Implement the lookup procedure for the case where the set of records is
; structured as a binary tree, ordered by the numerical values of the keys.

(define (key record)
  (car record))
(define (value record)
  (cdr record))
(define (make-record key value)
  (cons key value))

(define (entry records)
  (car records))
(define (left-branch records) (cadr records))
(define (right-branch records) (caddr records))
(define (make-records entry left right)
  (list entry left right))


(define (lookup lookup-key records)
  (cond ((null? records) false)
        ((= (key (entry records)) lookup-key) (value (entry records)))
        ((> (key (entry records)) lookup-key) (lookup lookup-key (left-branch records)))
        ((< (key (entry records)) lookup-key) (lookup lookup-key (right-branch records)))))

(define (insert record records)
  (cond ((null? records) (make-records record '() '()))
        ((= (key (entry records)) (key record)) records)
        ((> (key (entry records)) (key record)) (make-records (entry records) (insert record (left-branch records)) (right-branch records)))
        ((< (key (entry records)) (key record)) (make-records (entry records) (left-branch records) (insert record (right-branch records))))))

(define my-records (insert (make-record 5 500) 
                           (insert (make-record 6 600) 
                                   (insert (make-record 7 700) 
                                           (make-records (make-record 1 100) '() '())))))
my-records
(lookup 2 my-records)