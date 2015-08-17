#lang racket

; Exercise 2.35
; Redefine count-leaves from section 2.2.2 as an accumulation

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (node)
                         (if (pair? node)
                             (count-leaves node)
                             1))
                       t)))

(define x (cons (list 1 2) (list 3 4)))
(length x) ; 3
(count-leaves x) ; 4

(list x x) ; (((1 2) 3 4) ((1 2) 3 4))
(length (list x x)) ; 2
(count-leaves (list x x)) ; 8