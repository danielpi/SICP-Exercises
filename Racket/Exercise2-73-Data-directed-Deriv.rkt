#lang racket

; Exercise 2.73
; Section 2.3.2 described a program that performs symbolic differentiation:

;(define (deriv-old exp var)
;  (cond ((number? exp) 0)
;        ((variable? exp)
;         (if (same-variable? exp var) 1 0))
;        ((sum? exp)
;         (make-sum (deriv-old (addend exp) var)
;                   (deriv-old (augend exp) var)))
;        ((product? exp)
;         (make-sum (make-product (multiplier exp) 
;                                 (deriv-old (multiplicand exp) var))
;                   (make-product (deriv-old (multiplier exp) var)
;                                 (multiplicand exp))))
;        ;<more rules can be added here>
;        (else (error "unknown expression type: DERIV-OLD" exp))))
                   

; We can regard this program as performing a dispatch on the type of the
; expression to be differentiated. In this situation the "type tag" of the
; datum is the algebraic operator symbol (such as +) and the operation
; being performed is deriv. We can transform this program into data-directed
; style by rewriting the basic derivative procedure as

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               exp var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; a. Explain what was done above. Why can't we assimilate the predicates 
;    number? and variable? into the data-directed dispatch?

; b. Write the procedures for derivatives of sums and products, and the
;    auxiliary code required to install them in the table used by the 
;    program above.

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC"
                 (list op type-tags))))))

(define (install-sum-package)
  (define (=number? exp num) (and (number? exp) (= exp num)))
  (define (make-sum a1 a2) 
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (tag (list a1 a2)))))
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))
  (define (deriv-sum s var)
    (make-sum (deriv (addend s) var) 
              (deriv (augend s) var)))
  
  (define (tag x) (attach-tag '+ x))
  (put 'deriv '+ deriv-sum)
  (put 'make-sum '+
       (lambda (x y) (make-sum x y)))
  'done)

(define (make-sum x y)
  ((get 'make-sum '+) x y))

(define (install-product-package)
  (define (=number? exp num) (and (number? exp) (= exp num)))
  (define (make-product a1 a2) 
    (cond ((or (=number? a1 0) (=number? a2 0)) 0)
          ((=number? a1 1) a2)
          ((=number? a2 1) a1)
          ((and (number? a1) (number? a2)) (* a1 a2))
          (else (tag (list a1 a2)))))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))
  (define (deriv-product p var)
    (make-sum (make-product (multiplier p)
                            (deriv (multiplicand p) var))
              (make-product (deriv (multiplier p) var)
                            (multiplicand p))))
  (define (tag x) (attach-tag '* x))
  (put 'deriv '* deriv-product)
  (put 'make-product '*
       (lambda (x y) (make-product x y)))
  'done)

(define (make-product x y)
  ((get 'make-product '*) x y))

;(define (deriv x) (apply-generic 'deriv x))

(install-sum-package)
(install-product-package)


(deriv '(+ x 3) 'x)               ; 1
(deriv '(* x y) 'x)               ; 'y
(deriv '(* (* x y) (+ x 3)) 'x)  ; '(+ (* x y) (* y (+ x 3)))