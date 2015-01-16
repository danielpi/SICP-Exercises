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
;              to describe the behaviour of the following procedure.
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


; Exercise 1.7 The good-enough? test used in computing square roots will not 
; be very effective for finding the square roots of very small numbers. Also, 
; in real computers, arithmetic operations are almost always performed with 
; limited precision. This makes our test inadequate for very large numbers. 
; Explain these statements, with examples showing how the test fails for small 
; and large numbers.

; (define (good-enough? guess x)
;  (< (abs (- (square guess) x)) 0.001))

; Small numbers
; Our good enough function uses a fixed difference value of 0.001 to determine
; if the guess is good enough no matter what the value of x we are using is. If
; x is small then the square of the guess being within 0.001 of x is not a very 
; good indication that the guess shoudl be accepted.
; As an example the sqrt of 0.001 is 0.0316. However guesses in the range 0 to
; 0.0447 will be accepted as good enough
(good-enough? 0.00001 0.001)
(good-enough? 0.0447 0.001)
(sqrt 0.001)

; Large numbers

(sqrt 1000000000000) ; Evaluates quickly
;(sqrt 10000000000000) ; Evaluates slowly

; An alternative strategy for implementing  good-enough? is to watch how guess 
; changes from one iteration to the next and to stop when the change is a very 
; small fraction of the guess. Design a square-root procedure that uses this kind 
; of end test. Does this work better for small and large numbers?


(define (sqrt-iter2 prev-guess guess x)
  (if (good-enough2? prev-guess guess)
      guess
      (sqrt-iter2 guess (improve guess x) x)))
(define (good-enough2? prev-guess guess)
  (< (/ (abs (- guess prev-guess)) guess) 0.001))
(define (sqrt2 x)
  (sqrt-iter2 0.0 1.0 x))

(sqrt2 9)
(sqrt2 0.001)
(sqrt2 10000000000000)
; Yes it does work better for small and large numbers.


; Exercise 1.8 Newton's method for cube roots is based on the fact that if y 
; is an approximation to the cube root of x, then a better approximation is
; given by the value
; x/y ^2 + 2y 
; -----------
;      3
; Use this formula to implement a cube-root procedure analogous to the 
; square-root procedure.

(define (cube x)
  (* x (* x x)))
(define (cbrt-iter guess x)
  (if (cbrt-good-enough? guess x)
      guess
      (cbrt-iter (improve-cbrt guess x)
                 x)))
(define (improve-cbrt guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (cbrt-good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.00001))
(define (cbrt x)
  (cbrt-iter 1.0 x))

(cbrt 8)
(cbrt 0.001)
(cbrt 1000)


; 1.1.8 Procedures as Black-Box Abstractions
(define (square2 x) ( * x x))
(define (square3 x) (exp (double (log x))))
(define (double x) (+ x x))

(square2 4)
(square3 4)


; Internal definitions and block structure
; We can hide procedure definitions within other procedures so that only the 
; procedure that is required by the user will be accessible.
(define (sqrt3 x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(sqrt3 16)

; Nesting as shown above is called block structure. We can take it a step 
; further. Instead of passing x to each procedure we can turn x into a free
; variable for the internal definitions. This is called lexical scoping
(define (sqrt4 x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(sqrt4 64)


; 1.2 Procedures and the Processes they Generate
; 1.2.1 Linear Recursion and Iteration
; Factorial n! 
; Factorial can be thought of by noticing that n! == n * (n - 1)! . This is a recursive process

; (factorial 3)
; (* 3 (factorial 2))
; (* 3 (* 2 (factorial 1)))
; (* 3 (* 2 1)))
; (* 3 2)
; 6

(define (recursive-factorial n) 
  (if (= n 1)
      1
      (* n (recursive-factorial (- n 1)))))
(recursive-factorial 6)

; Note the ramped appearence of the calculation. This is due to the fact that the
; interpreter must keep track of a large amount of state as it progresses ???

; Lets take a different approach. We could maintain a running product along with a
; counter that counts from 1 to n
;
; product = counter * product
; counter = counter + a
;
; (factorial 3)
; (fact-iter 1 1 3)
; (fact-iter 1 2 3)
; (fact-iter 2 3 3)
; 6

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* product counter) (+ counter 1) max-count)))
(define (factorial-iterative n)
  (fact-iter 1 1 n))
(factorial-iterative 6)

(define (factorial-iterative2 n)
  (define (fact-iter product counter)
  (if (> counter n)
      product
      (fact-iter (* product counter) (+ counter 1))))
  (fact-iter 1 1))
(factorial-iterative2 6)

; Both approaches compute the same mathematical function and 
; requre the same number of steps which is proportional to n.

; The first approach has an expansion and then contraction. The expansion
; is due to a build up of deferred operations. The contraction is when
; the operations are performed. This is called a linear recursive process

; The second approach doesn't shrink or grow. At each step all we need
; to keep track of for any n are the current values of product, counter and max-count.
; This is a linear iterative process.

; Most popular languages are designed in such a way that the interpretation of any
; recursive procedure consumes an amount of memory that grows with the number
; of procedure calls. As such special looping constructs are required. Tail
; recursion can solve this problem though.


; Exercise 1.9 Each of the following two procedures defines a method for adding 
; two positive integrers in terms of the procedures inc, which increments its 
; argument by 1, and dec, which decrements its argument by 1.
(define (inc n)
  (+ n 1))
(define (dec n)
  (- n 1))
(define (+v1 a b)
  (if (= a 0)
      b
      (inc (+v1 (dec a) b))))
(define (+v2 a b)
  (if (= a 0)
      b
      (+v2 (dec a) (inc b))))

(+v1 4 5)
; (inc (+v1 3 5))
; (inc (inc (+v1 2 5)))
; (inc (inc (inc (!v1 1 5)))
; (inc (inc (inc (inc (+v1 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9


(+v2 4 5)
; (+v2 3 6)
; (+v2 2 7)
; (+v2 1 8)
; (+v2 0 9)
; 9

; +v1 is a recursive process. +v2 is an interative process


; Exercise 1.10 The following procedure computes a mathematical 
; function called Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
; What are the values of the following expressions?
(A 1 10)
; (A 0 (A 1 9))
; (A 0 (A 0 (A 1 8)))
; (A 0 (A 0 (A 0 (A 1 7))))
; (A 0 (A 0 (A 0 (A 0 (A 1 6))))
; ...
; 2 ^ 10
; 1024

(A 2 4)
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2)))
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2)))
; (A 1 (A 1 (A 0 (A 1 1))))
; (A 1 (A 1 (A 0 2))))
; (A 1 (A 1 4))
; (A 1 (A 0 (A 1 3)))
; (A 1 (A 0 (A 
; (2^4)^4

(A 3 3)
; 
; 134217728

(A 1 1)
(A 1 2)
(A 1 3)
(A 1 4)
(A 2 1)
(A 2 2)
(A 2 3)
(A 2 4)
(A 3 1)
(A 3 2)
(A 3 3)
; (A 3 4)


; Give concise mathematical definitions for the following
(define (f n) (A 0 n))
(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
; f(n) = 2n

(define (g n) (A 1 n))
(g 0)
(g 1)
(g 2)
(g 3)
(g 4)
; g(n) = 2^n

(define (h n) (A 2 n))
(h 0)
(h 1)
(h 2)
(h 3)
(h 4)
; h(n) = 2^(2^n)


; 1.2.2 Tree Recursion
; Fibonacci numbers
; 0 1 1 2 3 5 8 13 21
; 
;          / 0                       if n = 0
; Fib(n) = | 1                       if n = 1
;          \ Fib(n - 1) + Fib(n - 2) otherwise

(define (fib n)
  (cond ((= n 0) 0)
         ((= n 1) 1)
         (else (+ (fib (- n 1))
                  (fib (- n 2))))))
(fib 5)
; This procedure involves a lot of redundant computation. It can be shown
; that the procedure will calculate fib 1 and fib 0 fib(n + 1) times. This 
; is an exponential growth with respect to n. Memory grows linearly with the input.

; We can also process Fibonaccy numbers in an iterative process.
; a = a + b
; b = a



(define (fib1 n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n)) 
(fib1 5)

; The fib1 function operates in linear time. This difference between exponential and linear
; time makes a huge difference. For a value of n=64 fib1 takes a fraction of a second, fib
; on the other hand takes ages (I haven't waited long enough for it to finish yet.
(fib1 64)
;(fib 64)

; Tree-recursive processes aren't useless even though they may be less efficient. For instance 
; the version of fib listed above is a very direct translation from the mathematical formulation
; of the Fibonacci sequence into lisp and is therefore very easy to understand.


; Example: Counting change
; How many different ways can we make change of $1.00, given half-dollars, quarters, dimes, 
; nickels and pennies? More generally, can we write a procedure to computer the number of ways to
; change any given amount of money?

; Recursive
; The number of ways to change amount a using n kinds of coins equals
; - the number of ways to change amount a using all but the first kind of coin, plus
; - the number of ways to change amount a - d using all n kinds of coins, where d is the 
;   denomination of the first kind of coin.

( define (count-change amount)
   (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
(count-change 25)

; Exercise 1.11 A function is defined by the rule that f(n) = n if n < 3 and 
; f(n) = f(n - 1) + 2.f(n - 2) + 3.f(n - 3) if n >= 3. Write a procedure that computes
; f by means of a recursive process

; f(n) = | n                                     n <  3
;        | f(n - 1) + 2.f(n - 2) + 3.f(n - 3) if n >= 3

(define (ex1.11 n)
  (if (< n 3)
      n
      (+ (+ (* 1 (ex1.11 (- n 1))) 
            (* 2 (ex1.11 (- n 2)))
            (* 3 (ex1.11 (- n 3)))))))
(ex1.11 20)

; Write a procedure that computes f by means of an iterative process

(define (ex1.11-iter three two one count total)
  (if (> count total)
      one
      (ex1.11-iter two one (+ (+ (* 1 one) (* 2 two) (* 3 three))) (+ count 1) total)))
(define (ex1.11-iteration n)
  (if (< n 3)
      n
      (ex1.11-iter 0 1 2 3 n)))
(ex1.11-iteration 100)


; Exercise 1.12 Pascal's triangle
; The following pattern of numbers is called Pascal's triangle.
;     1
;    1 1
;   1 2 1
;  1 3 3 1
; 1 4 6 4 1
; The numbers at the edge of the triangle are all 1, and each number inside the triangle 
; is the sum of the two numbers above it. Write a procedure that computes elements of Pascal's 
; triangle by means of a recursive process.

; In other words, Computes an entry in the Pascal triangle given the row and column. Rows 
; start from 1, counting from above; columns start from 1 too, counting from left to right.

(define (pascal-recursive row col)
  (cond ((> col row) 0)
        ((= col row) 1)
        ((< row 2) 1)
        ((< col 2) 1)
        (else (+ (pascal-recursive (- row 1) (- col 1))
                 (pascal-recursive (- row 1) col)))))
(pascal-recursive 1 1)
(pascal-recursive 1 2)
(pascal-recursive 3 1)
(pascal-recursive 3 2) 
(pascal-recursive 3 3)
(pascal-recursive 4 1)
(pascal-recursive 4 2)
(pascal-recursive 4 3)
(pascal-recursive 4 4)
(pascal-recursive 10 5)



