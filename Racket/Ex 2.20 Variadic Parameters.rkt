#lang racket

; Exercise 2.20
; The procedures +, *, and list take arbitary numbers of arguments. One way to define such 
; procedures is to use define with dotted-tail notation. In a procedure definition, a parameter
; list that has a dot before the last parameter name indicates that, when the procedure is
; called, the initial parameters (if any) will have as values the initial arguments, as usual,
; but the final parameter's value will be a list of any remaining arguments. For instance,
; given the definition

; (define (f x y . z) <body>)

; the procedure f can be called with two or more arguments. If we evaluate
; (f 1 2 3 4 5 6)

; then in the body of f, x will be 1, y will be 2, and z will be the list (3 4 5 6). Given
; the definition

; (define (g . w) <body>)

; the procedure g can be called with zero or more arguments. If we evaluate
; (g 1 2 3 4 5 6)

; then in the body of g, w will be the list (1 2 3 4 5 6)

; Use this notation to write a procedure same-parity that takes one or more integers and
; returns a list of all the arguments that have the same even-odd parity as the first argument.
; for example,

(define (even? n)
  (= (remainder n 2) 0))
(define (odd? n)
  (not (even? n)))
(odd? 1)
(odd? 2)

(define (parity-match? v1 v2)
    (if (even? v1)
        (even? v2)
        (odd? v2)))
(parity-match? 1 2)
(parity-match? 2 4)

(define (reverse list)
  (define (reverse-iter list reverse-list)
    (if (null? list)
        reverse-list
        (reverse-iter (cdr list) (cons (car list) reverse-list))))
  (reverse-iter list '()))


(define (same-parity . data)
  (define (iter input output first)
    (if (null? input)
        output
        (iter (cdr input) 
              (if (parity-match? (car input) first) 
                  (cons (car input) output)
                  output) 
              first)))
  (reverse (iter data '() (car data))))

(same-parity 1 2 3 4 5 6 7) ; returns (1 3 5 7)
(same-parity 2 3 4 5 6 7)   ; returns (2 4 6)