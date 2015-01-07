#lang racket

; SICP Exercise 1.1
10                               ; 10
(+ 5 3 4)                        ; 12
(- 9 1)                          ;  8
(/ 6 2)                          ;  3
(+ (* 2 4) (- 4 6))              ;  6
(define a 3)                     
(define b (+ a 1))
(+ a b (* a b))                  ; 19
(= a b)                          ; #f
(if (and (> b a) (< b (* a b)))
    b
    a)                           ;  4 (b)
(cond ((= a 4) 6)             
      ((= b 4) (+ 6 7 a))
      (else 25))                 ; 16 (= b 4)
(+ 2 (if (> b a) b a))           ;  6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))                      ; 16

; Exercise 1.2
(/ (+ 5 (+ 4 (- 2 (- 3 (+ 6 (/ 4 5))))))
   (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3 Define a procedure that takes three numbers as arguments and 
;              returns the sum of the squares of the two larger numbers.
(define (square x) (* x x))
(define (sumOfSquares x y) (+ (square x) (square y)))
(define (!= a b) (if (= a b) #f #t))



(define (sumOfSquaresOfTwoLargest a b c) (cond ((= (min a b c) a) (sumOfSquares b c))
                                               ((= (min a b c) b) (sumOfSquares a c))
                                               ((= (min a b c) c) (sumOfSquares a b))))
(sumOfSquares 6 4)
(sumOfSquaresOfTwoLargest 6 2 4)
;(sumOfSquaresOfTwoLargest 4 4 4)