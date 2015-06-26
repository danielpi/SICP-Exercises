#lang racket

; Exercise 2.58
; Suppose we want to modify the differentiation program so that it works with ordinary
; mathematical notation, in which + and * are infix rather than prefix operatiors. Since
; the differentiation program is defined in therms of abstract data, we can modify it to 
; work with different representations of expressions solely by changing the predicates, 
; selectors, and constructors on which the differentiatior is to operate.

; a. Show how to do this in order to differentiate algebraic expressions presented
;    in infix form, such as (x + (3 * (x + (y + 2)))). To simplify the task, assume that + 
;    and * always take two arguments and that expressions are fully parenthesized.

(define (accumulate op initial sequence) 
  (if (null? sequence)
             initial
             (op (car sequence)
                 (accumulate op initial (cdr sequence)))))

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
(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))

(define (make-product a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list a1 '* a2))))
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

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