\ 1.2 Procedures and the Processes they Generate
\ 1.2.1 Linear Recursion and Iteration
\ Factorial n!
\ Factorial can be thought of by noticing that  n! == n * (n - 1)! . This is a
\ recursive process

\ 3 FACTORIAL
\ 2 FACTORIAL 3 *
\ 1 Factorial 2 * 3 *
\ 1 2 * 3 *
\ 2 3 *
\ 6

\ We don't appear to have compiler support for recursive function calls

: FACTORIAL ( n -- n! )   
	1 SWAP  ( 1 n )
	1 MAX   ( 1 n )
	1+ 2 	( 1 n+1 2 )
	DO 
	 	I * ( 1 n * n-1 * n-2 * ... 2 * )
	LOOP
	;
3 FACTORIAL .