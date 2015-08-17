#lang racket

; Exercise 2.54
; Two lists are said to be equal? if they contain equal elements arranged in the
; same order. For example, 

(equal? '(this is a list) '(this is a list))

; is true, but

(equal? '(this is a list) '(this (is a) list))

; is false. To be more precise, we can define equal? recursively in terms of the basic
; eq? equality of symbols by saying that a and b are equal? if the are both symbols and
; the symbols are eq?, or if they are both lists such that (car a) is equal? to (car b)
; and (cdr a) is equal? to (cdr b). Using this idea, implement equal? as a procedure.

(define (myequal? x y) 
   (cond ((and (null? x) (null? y)) #t) 
         ((and (symbol? x) (symbol? y)) (eq? x y)) 
         ((and (pair? x) (pair? y)  
               (eq? (car x) (car y)) (myequal? (cdr x ) (cdr y))) #t) 
         (else #f))) 

(display "1: ") (myequal? '(this is a list) '(this is a list))
(display "2: ") (myequal? '(this is a list) '(this (is a) list))

(display "3: ") (myequal? (list 1 2 3) (list 1 2 3))
(display "4: ") (myequal? (list 1 2 3) (list 1 2 3 4))

(display "5: ") (myequal? '() '())
(display "6: ") (myequal? '() '(a))