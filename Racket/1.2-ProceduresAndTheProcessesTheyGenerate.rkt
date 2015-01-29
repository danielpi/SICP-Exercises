#lang racket

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
(factorial-iterative 5)

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

; Exercise 1.14 Draw the tree illustrating the process generated by the count-change procedure 
; of section 1.2.2 in making change for 11 cents. What are the orders of growth of the space 
; and number of steps used by this process as the amount to be changed increases?

;(define (count-change amount)
;   (cc amount 5))
;(define (cc amount kinds-of-coins)
;  (cond ((= amount 0) 1)
;        ((or (< amount 0) (= kinds-of-coins 0)) 0)
;        (else (+ (cc amount (- kinds-of-coins 1))
;                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))
;(define (first-denomination kinds-of-coins)
;  (cond ((= kinds-of-coins 1) 1)
;        ((= kinds-of-coins 2) 5)
;        ((= kinds-of-coins 3) 10)
;        ((= kinds-of-coins 4) 25)
;        ((= kinds-of-coins 5) 50)))
;(count-change 25)
(count-change 11)

;                              11[50 25 10 5 1]
;                                      |
;                        11[25 10 5 1] - 0
;                              |
;                  11[10 5 1] -  0
;                      |
;            11[5 1]--------------------- 1[10 5 1]
;               |                             |
;        11[1] ----- 6[5 1]            1[5 1] - 0
;         |            |                  |
;   10[1] - 0     6[1] ---- 1[5 1]   1[1] -  0
;      |           |           |       |
; 9[1] - 0    5[1] - 0    1[1] - 0   0 - 1
;      |           |           |
; 8[1] - 0    4[1] - 0       0 - 1
;      |           |
; 7[1] - 0    3[1] - 0
;      |           |
; 6[1] - 0    2[1] - 0
;      |           |
; 5[1] - 0    1[1] - 0
;      |           |
; 4[1] - 0       0 - 1
;      |
; 3[1] - 0
;      |
; 2[1] - 0
;      |
; 1[1] - 0
;      |
;    0 - 1
 
; (cc n 1) generates 2*n evaluations of cc so the time order of growth is O(n). The 
; memory used is the call stack which is proportionaly to the depth of the tree, O(n).

; (cc n 1) = O(n)
; (cc n 2) = (+ (cc n 1) (cc (- n 1) 2)) = (+ O(n) (cc (- n 1) 1) (cc (- n 5) 2))

; So for each new coin added we add another O(n) process. Giving an overall order
; of growth of O(n^k) where k = number of coins.


; Exercise 1.15
; The sine of an angle (specified in radians) can be computed by making use of 
; the approximation sin x ~ x if x is sufficiently small, and the trigonometric identity
;
; sin r = 3 sin (r/3) - 4 sin^3 (r/3)
;
; to reduce the size of the argument of sin. (For purposes of this exercise an angle 
; is considered "sufficiently small" if its magnitude is not greater than 0.1 radians. 
; These ideas are incorporated in the following procedures:

(define (cube x) (* x x x))
(define (p2 x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p2 (sine (/ angle 3.0)))))
(sine 12.15)

; a) How many times is the procedure p2 applied when (sine 12.15) is evaluated?
; 12.15 / 3^n < 0.1
; 0.1 * 3^n > 12.15
; 3^n > 121.5
; n * log(3) > log(121.5)
; n <  log(121.5) / log(3)
; n = 5
(ceiling (/ (log (/ 12.15 0.1)) (log 3)))

; b) What is the order of growth in space and number of steps (as a function of a)
; used by the process generated by the sine procedure when (sine a) is evaluated?

; The number of steps required to calculate sine a can be calculated as below
;        (ceiling(/ (log (/ a 0.1)) (log 3)))
; Which gives us an order of growth for size and space as 
;        O(log(n))


; 1.2.4 Exponentiation
; Consider the problem of computing the exponential of a given number. We would
; like a procedure that takes as arguments a base b and a positive integer exponent n 
; that computes b^n. This could be done via the recursive definition

; b^n = b.b^(n-1)
; b^0 = 1

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
(expt 2 16)

; This is a linear recursive process, which requires O(n) steps and O(n) space.
; This can be calculated in an iterative process using the following

(define (expt2 b n)
  (define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b 
                   (- counter 1) 
                   (* b product))))
  (expt-iter b n 1))
(expt2 5 3)

; This version requires O(n) steps and O(1) space.

; We can computer exponentials in fewer steps by using successive squareing. For instance
; rather than computing b^8 as
;
; (b . (b . (b . (b . (b . (b . (b .b)))))))
;
; We can compute it using three multiplications
;
; b^2 = b . b
; b^4 = b^2 . b^2
; b^8 = b^4 . b^4

; This method works fine for exponents that are powers of 2. We can also take advantage of
; successive squaring in computing exponentials in general if we use the rule
; b^n = (b^(b/2))^2 if n is even
; b^n = b.b^(n-1)   if n is odd

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))
(define (square n) (* n n))
(fast-expt 5 16)

; fast-expt grows logarithmically with n in both space and number of steps. To see this
; observe that computing b^2n requires only one more multiplication than computing b^n.
; The size of the exponent we can compute therefore doubles with every new multiplication.
; Hence O(log(n))


; Exercise 1.16
; Design a procedure that evolves an iterative exponentiation process that uses successive
; squaring and uses a logarithmic number of steps, as does fast-expt. 

; Hint: Using the observation that 
;
; (b^(n/2))^2 = (b^2)^(n/2)
;
; keep, along with 
; - the exponent n 
; - the base b 
; - an additional state variable a, 

; and define the state transformation in such a way that the product a.b^n is 
; unchanged from state to state. 

; At the beginning of the process a is taken to be 1, and the answer is given by the value of 
; a at the end of the process. 

; In general, the technique of defining an invariant quantity that remains unchanged from 
; state to state is a powerful way to thing about the design of iterative algorithms.

(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter a (square b) (/ n 2)))
        (else (fast-expt-iter (* a b) b (- n 1)))))
(define (fast-expt2 b n)
  (fast-expt-iter 1 b n))

(fast-expt2 2 10)


; Exercise 1.17
; Using addition, double and halve design a multiplication procedure analoguous to fast-expt
; tha uses a logarithmic number of steps.

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-multi-recursive a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-multi-recursive a (halve b))))
        (else (+ a (fast-multi-recursive a (- b 1))))))
(fast-multi-recursive 3 4)
(fast-multi-recursive 3 101)


; Exercise 1.18
; Devise a procedure that generates an iterative process for 
; multiplying two integers in terms of adding, doubling and halving
; that uses a logarithmic number of steps

(define (fast-multi-iter a b c)
  (cond ((= b 1) (+ a c))
        ((even? b) (fast-multi-iter (double a) (halve b) c))
        (else (fast-multi-iter a (- b 1) (+ c a)))))
(define (fast-multi a b)
  (fast-multi-iter a b 0))

(fast-multi 3 4)
(fast-multi 15 101)


; Exercise 1.19
; From 1.2.2 recall the transformation of state variables a and b in fib-iter
;
; a <- a + b
; b <- a
;
; Call this transformation T. Applying T over and over n times starting with 1 and 0
; produces the pair Fib(n + 1) and Fib(n). The Fibonaci numbers are generated by 
; applying T^n with the starting pair (1, 0).
;
; Now consider Tpq
;
; a' <- bq + aq + ap
; b' <- bp + aq
;
; Show that if we apply such a transform twice, the effect is the same as using a 
; single transform Tp`q` and compute p` and q` in terms of p and q. This gives us
; an explicit way to square these transformations and thus we can compute T^n using 
; successive squaring.

; T^2
; a' <- (q + p)a + qb
; b' <- qa + bp
; a" <- (p + q)(p + q)a + q(q

(define (fib3 n)
  (fib-iter3 1 0 0 1 n))
(define (fib-iter3 a b p q count)
  (cond ((= count 0) b)
        ((even? count) (fib-iter3 a
                                  b
                                  (+ (square p) (square q))
                                  (+ (* 2 p q) (square q))
                                  (/ count 2)))
        (else (fib-iter3 (+ (* b q) (* a q) (* q p))
                         (+ (* b p) (* a q))
                         p q
                         (- count 1)))))
(fib3 6)


; 1.2.5 Greatest Common Divisors
; The greatest commong divisor of two integers is the largest integer that divides
; both a and b with no remainder. GCD of 16 and 28 is 4.

; Euclid's Algorithm
; If r is the remainder when a is divided by b, then the common divisors of a and b 
; are the same as the common divisors of b and r.
;
; GCD(a, b) == GCD(b, r)
;
; Example
; GCD(206,40) = GCD(40, 6)
;             = GCD(6, 4)
;             = GCD(4, 2)
;             = GCD(2, 0)
;             = 2

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(gcd 206 40)
(gcd 28 16)
(gcd 7 300)

; This generates an iterative process whose number of steps grows at
; a logarithmic rate.

; Lame's Theorem
; If Euclid's Algorithm requires k steps to compute the GCD of some 
; pair, then the smaller number in the pair must be greater than or 
; equal to the kth Fibonacci number.

; Exercise 1.20
; The process that a procedure generates is dependent on the rules used by the 
; interpreter that runs the procedure. How would GCD be processed using normal-
; order evaluation?

(gcd 206 40)
(if (= 40 0) 206 (gcd 40 (remainder 206 40)))
(gcd 40 (remainder 206 40))
(if (= (remainder 206 40) 0) 40 (gcd (remainder 206 40)(remainder 40 (remainder 206 40))))
;(if (= 6 0) ...)  1 remainder
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
(if (= (remainder 40 (remainder 206 40)) 0) 
    (remainder 206 40) 
    (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) 
;(if (= 4 0) ...) 1 + 2 remainder
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40))  (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
;(if (= 2 0) ...) 3 + 4 remainder
(gcd 
 (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 
 (remainder (remainder 40 (remainder 206 40))  (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
(if (= (remainder (remainder 40 (remainder 206 40))  (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0)
    (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40))  (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
         (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40))  (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
;(if (= 0 0) ...) 7 + 7 remainder
(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
; 14 + 4 remainder
; Until the last step, when 4 remainders are calculated as part of evaluating a, the remainders in the if 
; statements are all that are evaluated.

; How many remainder operations are performed in applicative order process?
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
2
; there are 4 remainder operations.


; 1.2.6 Example: Testing for Primality
; Two methods of checking the primality of an integer, one with order of growth
; O(n^0.5) and a probabilistic algorithm with order of growth O(log(n)).

; Searching for divisors
; The following program finds the smallest integral divisor of a given number n
; by testing n for divisibility by successive integers starting with 2.

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(smallest-divisor 4)
(smallest-divisor 7)
(smallest-divisor 3127)
(smallest-divisor 23113)

; We can test if a number is prime if it's smallest divisor is itself

(define (prime? n)
  (= n (smallest-divisor n)))
(prime? 7)
(prime? 23113)

; The end test for find-divisor is based on the fact that if n is not 
; prime it must have a divisor less than or equal to n^0.5. This means the 
; algorithm need only test divisors between 1 and n^0.5. Consequently, the 
; number of steps required to identify n as prime will have order of growth
; O(n^0.5).

; The Fermat test (Fermat's Little Theorem)
; If n is a prime number and a is any positive integer less than n, then a
; raised to the nth power is congruent to a modulo n.

; (Two numbers are said to be congruent modulo n if they both have the 
; same remainder when divided by n. The remainder of a number a when divided 
; by n is also referred to as the remainder of a modulo n, or simply as
; a modulo n)

; If n is not prime most of the numbers a < n will not satisfy the above
; relation. This leads to the following algorithm
; - Given a number n
; - pick a random number a < n
; - compute the remainder of a^n modulo n
; - If the result is not equal to a then n is certainly not prime
; - If it is then n might be prime
; - Pick another a and test again

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
(fast-prime? 7 3)
(fast-prime? 1235 10)
(fast-prime? 313 4)

; Exercise 1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

; Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-inexact-milliseconds) start-time))
      (display " ")))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
; Using the procedure above write a procedure search-fo-primes that checks 
; the primality of consecutive odd integers in a specified range. 
(timed-prime-test 31123)

(define (search-for-primes a b)
  (cond ((> a b) (display " done "))
        ((even? a) (search-for-primes (+ a 1) b))
        (else (timed-prime-test a) (search-for-primes (+ a 2) b))))


; Use your procedure to find the three smallest primes larger than
; - 1000
; - 10000
; - 100000
; - 1000000

(search-for-primes 1000 1030)
; 1009 *** 0.004150390625
; 1013 *** 0.00390625
; 1019 *** 0.00390625
(/ (+ 0.004150390625 0.00390625 0.00390625) 3)
; Average time 0.00398

(search-for-primes 10000 10050)
; 10007 *** 0.01220703125
; 10009 *** 0.011962890625
; 10037 *** 0.012939453125
(/ (+ 0.01220703125 0.011962890625 0.012939453125) 3)
; Average time 0.012369

(search-for-primes 100000 100050)
; 100003 *** 0.037841796875
; 100019 *** 0.0439453125
; 100043 *** 0.033935546875
(/ (+ 0.037841796875 0.0439453125 0.033935546875) 3)
; Average time 0.03857

(search-for-primes 1000000 1000050)
; 1000003 *** 0.1181640625
; 1000033 *** 0.10498046875
; 1000037 *** 0.10400390625
(/ (+ 0.1181640625 0.10498046875 0.10400390625) 3)
; Average time 0.109049

; Note the time needed to test each prime. Since the testing algorithm has an order of 
; growth of O(n^0.5) you should expect primes around 10,000 to take about 10^0.5 times
; as long to test for as for primes around 1000.

;         | Actual Times |    1000 |   10000 |  100000 | 1000000 |
;    1000 |    0.00398   | 0.00398 | 0.00390 | 0.00385 | 0.00344 |
;   10000 |    0.01236   | 0.01258 | 0.01236 | 0.01219 | 0.01090 |
;  100000 |    0.03857   | 0.0398  | 0.03908 | 0.03857 | 0.03448 |
; 1000000 |    0.10905   | 0.12585 | 0.1236  | 0.12196 | 0.10905 |

; Does your timing data bear this out?                                                  Yes
; How well do the data for 100,000 and 1,000,000 support the n^0.5 prediction?    Very well
; Is the result compatible with the notion that programs on your machine run in 
; time proportional to the number of steps required for the computation?                Yes

