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


; Exercise 1.4 Observe that our model of evaluation allows for combinations 
;              whose operators are compound expressions. Use this observation 
;              to describe the behaiour of the following procedure.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 4 5)
(a-plus-abs-b 4 -5)
; In the procedure above the operator used for combining the values of a and 
; b is choosen at runtime based on the evaluation of the computation of 
; whether b is positive or negative.


; Exercise 1.5 Ben Bitdiddle has invented a test to determine whether the interpreter
;              he is faced with is using applicative-order evaluation or normal-order 
;              evaluation. He defines the following two procedures:
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
; Then he evaluates the expression
;(test 0 (p))

;              What behaviour will Ben observe with an interpreter that uses 
;              applicative-order evaluation?
; (test 0 (p))
; (test 0 (p))
; (test 0 (p))
; (test 0 (p))
; 
; Infinite loop. The (p) is repeditively expanded to itself.

;              What behaviour will he observe with an interpreter that uses 
;              normal-order evaluation?
; (test 0 (p))
; (if (= x 0) 0 y))
; (if (= 0 0) 0 (p)))
; (if #t 0 (p)))
; 0
; Result is 0. The procedure is evaluated step by step to get the result.


; 1.1.7 Example: Square Roots by Newton's Method
; Functions are declarative knowledge and procedures are imperative knowledge.
; Meaning that functions describe functions of things and procedures describe
; how to do things.

; How does one compute square roots?
; Guess a value, y, for the value of the square root of a number x. Find the
; quotient of x divided by the guess. Average the quotient and the guess.
; Continue till you have an accurate enough answer.

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))


; Exercise 1.6 Alyssa P. Hacker doesn't see why if needs to be provided as
; a special form. "Why can't I just define it as an ordinary procedure in 
; terms pf cond?" she asks. Alyssa's friend Eva Lu Actor claims this can 
; indeed be done and she defines a new version of if:
(define (new-if predicate then-clause else-clause) 
  (cond (predicate then-clause)
      (else else-clause)))

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)
; Delighted, Alyssa uses new-if to rewrite the square-root program:
(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x)
                        x)))
(define (new-sqrt x) (new-sqrt-iter 1 x))
;(new-sqrt 9)
; If we run the line above the interpreter loops forever. The if statement
; is a special form that evaluates the predicate and then evaluates one,
; but not both of the consequant or alternative. new-if on the other hand
; is a procedure so the predicate, consequant and alternative are all evaluated
; before going to the next step. At this point new-sqrt-iter is evaluated again
; which leads to the infinite loop.
