#lang racket

; Exercise 2.39
; Complete the following definitions of reverse in terms of fold-right and fold-left

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (reverse1 sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(reverse1 (list 1 2 3))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse2 sequence)
  (fold-left (lambda (x y) (append (list y) x)) '() sequence))

(reverse2 (list 1 2 3))