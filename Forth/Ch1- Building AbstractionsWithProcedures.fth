\ SICP Exercise 1.1
10 . 							\ 10  ok
5 3 4 + + . 					\ 12  ok
9 1 - . 						\ 8  ok
6 2 / . 						\ 3  ok
4 6 - 2 4 * + . 				\ 6  ok
: a 3 ; 						\ ok
a . 							\ 3  ok
: b  
 a 1 + 
 ; 								\ ok
b . 							\ 4  ok
a b * a b + + . 				\ 19  ok
a b = . 						\ 0  ok

a b *  							\ ok
b <  							\ ok
b a >  							\ ok
AND [IF] 
  b .
[ELSE] 							\ ok
 a . 							\ 3  ok
[ENDIF]  						\ ok

\ I don't think we have a COND or CASE equivilent in our FORTH'
\ (cond ((= a 4) 6)             
\       ((= b 4) (+ 6 7 a))
\       (else 25))                 ; 16 (= b 4)


\ (+ 2 (if (> b a) b a))           ;  6
\ (* (cond ((> a b) a)
\          ((< a b) b)
\          (else -1))
\    (+ a 1))                      ; 16

\ Exercise 1.2
2 7 - 6 2 - *
4 5 / 6 + 3 - 2 - 4 + 5 +
/ .								\ -2  Error as it is an int ok


\ Exercise 1.3 - Define a procedure that takes three numbers as 
\                arguments and returns the sum of the squares of the two
\                larger numbers
: SQUARE ( x --- x^2 )
	DUP *
	;
: SUMOFSQUARES ( x y --- x^2 + y^2 )
	DUP * SWAP DUP * +
	;
6 4 SUMOFSQUARES .

: SUMOFSQUARESOFTWOLARGEST ( x y z ---  A^2 + B^2 )
	2DUP MAX 			\ x y z max(yz)
	ROT ROT				\ x max(yz) y z
	MIN ROT 			\ max(yz) min(yz) x
	MAX SUMOFSQUARES
	;	
6 2 4 SUMOFSQUARESOFTWOLARGEST .


\ Exercise 1.4 Observe that our model of evaluation allows for combinations
\              whose operators are compond expressions. Use this observation
\              to describe the behaviour of the following procedure.
: A-PLUS-ABS-B  ( a b --- a + abs b )
	DUP 0 > IF
		+
	ELSE
		-
	ENDIF
	;
4 5 A-PLUS-ABS-B
4 -5 A-PLUS-ABS-B


\ Exercise 1.5 Ben Bitdiddle has invented a test to determine whether the interpreter
\              he is faced with is using applicative-order evaluation or normal-order
\              evaluation. He defines the following two procedures
\ I don't think this is going to work for FORTH as I don't know how to pass
\ a 


\ Example 1.1.7 Example: Square Roots by Newton's Method
\ Functions are declarative knowledge and procedures are imperative knowledge.
\ Meaning that functions describe functions of things and procedures describe
\ how to do things.

\ How does one computer square roots?
\ Guess a value, y, for the value of the square root of a number x. Find the
\ quotient of x divided by the guess. Average the quotient and the guess.
\ Continue till you have an accurate enough answer.

: AVERAGE ( x y --- x + y / 2 )
	+ 2 /
	;
: IMPROVE ( guess x --- improved_guess )
	SWAP DUP ROT / AVERAGE
	;
	: GOOD-ENOUGH ( guess x --- bool )
	SWAP SQUARE - ABS 0.001






