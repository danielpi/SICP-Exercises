#lang racket

; Exercise 2.58b
; Suppose we want to modify the differentiation program so that it works with ordinary 
; mathematical notation, in which + and * are infix rather than prefix operatiors. Since
; the differentiation program is defined in terms of abstract data, we can modify it to 
; work with different representations of expressions solely by changing the predicates,
; selectors, and constructors that define the representation of the algebraic expressions
; on which the differentiator is to operate.

; b. The problem becomes substantially harder if we allow standard algebraic notation,
;    such as (x + 3 * (x + y + 2)), which drops unnecessary parentheses and assumes
;    that multiplication is done before addition. Can you design appropriate predicates,
;    selectors, and constructors for this notation such that our derivative program
;    still works?

(define (accumulate op initial sequence) 
  (if (null? sequence)
             initial
             (op (car sequence)
                 (accumulate op initial (cdr sequence)))))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (memr item x)
  (if (pair? x)
      (reverse (memq item (reverse x)))
      x))

(define (operation expr)
  (cond ((memq '+ expr) '+)
        ((memq '* expr) '*)
        ((memq '** expr) '**)
        (else (error "Unknown operation: OPERATION" expr))))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? x) (eq? '+ (operation x)))

(define (addend s) 
  (let ((result (reverse (cdr (reverse (memr '+ s))))))
    (if (= (length result) 1)
        (car result)
        result)))
    
(define (augend s)
  (let ((result (cdr (memq '+ s))))
    (if (= (length result) 1)
        (car result)
        result)))


(define (make-product a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list a1 '* a2))))

(define (product? x) (eq? '* (operation x)))

(define (multiplier p)
  (let ((result (reverse (cdr (reverse (memr '* p))))))
    (if (= (length result) 1)
        (car result)
        result)))
    
(define (multiplicand p)
  (let ((result (cdr (memq '* p))))
    (if (= (length result) 1)
        (car result)
        result)))

; Exponentiation 
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list base '** exponent))))
(define (base x) (car x))
(define (exponent x) (caddr x))
(define (exponentiation? x) (and (pair? x) (eq? (cadr x) '**)))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product 
          (make-product (exponent exp)
                        (make-exponentiation (base exp) (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))


; Tests
(deriv '(x + 3) 'x)                   ; 1
(deriv '(x * y) 'x)                   ; 'y
(deriv '((x * y) * (x + 3)) 'x)       ; '(+ (* x y) (* y (+ x 3)))
(deriv '(x ** 2) 'x)                  ; '(* 2 x)
(deriv '(x + (3 * (x + (y + 2)))) 'x) ; 4
(deriv '(x + 3 * (x + y + 2)) 'x)     ; 4

; I cheated and copied this answer from SICP Answers, so should put in a bit of effort
; to write up my understanding of it.

; To start with we just look at the top most layer of the expression in each pass. We aren't
; building a big tree and recursing to the depths at the outset. So we want to find the operator
; with the least precedence first and split off everything before it and after it. We then evalate
; the before and after bits first (this means that the operator with the least precedence will 
; do its thing last). 

; Did a bit by myself adding memr to simplify multiplier and addend and made it handle exponentials 
; too

(sum? '(x * 3 + (x + y + 2)))
(product? '(x * 3 + (x + y + 2)))