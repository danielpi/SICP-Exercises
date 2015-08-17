#lang racket

; Exercise 2.56
; Show how to extend the basic differentiator to handle more kinds of expressions. For instance,
; implement the differentiation rule

;                            d(u^n)         du
;                            ------ = nu^n-1--
;                              dx           dx

; by adding a new clause to the deriv program and defining appropriate procedures exponentiation?,
; base, exponent, and make-exponentiation.) Build in the rules that anything raised to the power
; 0 is 1 and anything raised to the power 1 is the thing itself.

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-product a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list '* a1 a2))))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))


; Exponentiation 
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))
(define (base x) (cadr x))
(define (exponent x) (caddr x))
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))


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
(deriv '(+ x 3) 'x)               ; 1
(deriv '(* x y) 'x)               ; 'y
(deriv '(* (* x y) ( + x 3)) 'x)  ; '(+ (* x y) (* y (+ x 3)))
(deriv '(** x 2) 'x)              ; '(* 2 x)