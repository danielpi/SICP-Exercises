#lang racket

; Exercise 2.73

; d. In this simple algebraic manipulator the type of an expression is the algebraic 
;    operator that binds it together. Suppose, however, we indexed the procedures in
;    the opposite way, so that the dispatch line in deriv looked like 

;    ((get (operator exp) 'deriv) (operands exp) var)

;    What corresponding changes to the derivative system are required?




(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv) exp var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))



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
  (put '+ 'deriv deriv-sum)
  (put '+ 'make-sum
       (lambda (x y) (make-sum x y)))
  'done)

(define (make-sum x y)
  ((get '+ 'make-sum ) x y))

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
    ;(display p) (display (multiplier p)) (display (multiplicand p))
    ;(newline)
    (make-sum (make-product (multiplier p)
                            (deriv (multiplicand p) var))
              (make-product (deriv (multiplier p) var)
                            (multiplicand p))))
  (define (tag x) (attach-tag '* x))
  (put '* 'deriv deriv-product)
  (put '* 'make-product
       (lambda (x y) (make-product x y)))
  'done)

(define (make-product x y)
  ((get '* 'make-product) x y))

(install-sum-package)
(install-product-package)


(deriv '(+ x 3) 'x)               ; 1
(deriv '(* x y) 'x)               ; 'y
(deriv '(* (* x y) (+ x 3)) 'x)   ; '(+ (* x y) (* y (+ x 3)))

; The only change that was required was to switch the order of the op and type in the
; put calls.