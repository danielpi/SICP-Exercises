#lang racket

; 2.3.2 Example: Symbolic Differentiation

; As an illustration of symbol manipulation and a further illustration of data abstraction,
; consider the design of a procedure that performs symbolic differentiation of algebraic 
; expressions. We would like the procedure to take as arguments an algebraic expression and a 
; variable and to return the derivative of the expression with respect to the variable. For
; example, if the arguments to the procedure are ax^2 + bx + c and x, the procedure should
; return 2ax + b. Symbolic differentiation is of special historical significance in Lisp. It
; was one of the motivating examples behind the development of a computer language for symbol
; manipulation. Furthermore, it marked the beginning of the line of research that led to the
; development of powerful systems for symbolic mathematical work, which are currently being
; used by a growing number of applied mathematicians and physicists.

; In developing the symbolic-differentiation program, we will follow the same strategy of
; data abstraction that we followed in developing the rational-number system of Section 2.2.2.
; That is, we will first define a differentiation algorithm that operates on abstract objects
; such as "sums," "products," and "variables' without worrying about how these are to be
; represented. Only afterward will we address the representation problem.

; The differentiation program with abstract data
; In order to keep things simple, we will consider a very simple symbolic differentiation
; program that handles expressions that are built up using only the operations of addition
; and multiplication with two arguments. Differentiation of any such expression can be 
; carried out by applying the following reduction rules:

;           dc
;           -- = 0,  for c a constant or a variable different from x,
;           dx
;
;                                 dx
;                                 -- = 1
;                                 dx
;
;                           d(u + v)   du   dv
;                           -------- = -- + --
;                              dx      dx   dx
;
;                              d(uv)    dv    du
;                              ----- = u-- + v--
;                                dx     dx    dx

; Observe that the latter two rules are recursive in nature. That is, to obtain the derivative 
; of a sum we first find the derivatives of the terms and add them. Each of the terms may in
; turn be an expression that needs to be decomposed. Decomposing into smaller and smaller pieces
; will eventually produce pieces that are either constants or variables, whose derivatives will
; be either 0 or 1

; To embody these rules in a procedure we indulge in a little wishful thinking, as we did in
; designing the rational-number implementation. If we had a means for representing algebraic
; expressions, we should be able to tell whether an expression is a sum, a product, a constant,
; or a variable. We should be able to extract the parts of an expression. For a sum, for example
; we want to be able to extract the addend (first term) and the augend (second term). We should 
; also be able to construct expressions from parts. Let us assume that we already have procedures
; to implement the following selectors, constructors, and predicates:

; (variable? e)                    Is e a variable
; (same-variable? v1 v2)           Are v1 and v2 the same variable?
; (sum? e)                         Is e a sum?
; (addend e)                       Addend fo the sum e
; (augend e)                       Augend of the sum e
; (make-sum a1 a2)                 Construct the sum of a1 and a2
; (product? e)                     Multiplier of the product e
; (multiplier e)                   Multiplier of the product e
; (multiplicand e)                 Multiplicand of the product e
; (make-product m1 m2)             Construct the product of m1 and m2

; Using these, and the primitive predicate number?, which identifies numbers, we can express
; the differentiation rules as the following procedures

; <See below after the Representing Algebraic Expressions section>

; (define (deriv exp var)

; This deriv procedure incorporates the complete differentiation algorithm. Since it is expressed 
; in terms of abstract data, it will work no matter how we choose to represent algebraic expressions,
; as long as we design a proper set of selectors and constructors. This is the issue we must address
; next.


; Representing Algebraic Expressions
; We can imagine many ways to use list structure to represent algebraic expressions. For example
; we could use lists of symbols that mirror the usual algebraic notation, representing ax + b as
; the list (a * x + b). However, one expecially straightforward choice is to use the same
; parenthesized prefix notation that Lisp uses for combinations; that is, to represent ax + b
; as (+ (* a x) b). Then our data representation for the differentiation problem is as follows:

; - The variables are symbols. They are identified by the primitive predicate symbol?
(define (variable? x) (symbol? x))

; - Two variables are the same if the symbols representing them are eq?
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; - Sums and products are constructed as lists:
(define (make-sum1 a1 a2) (list '+ a1 a2))
(define (make-product1 a1 a2) (list '* a1 a2))

; - A sum is a list whose first element is the symbol +:
(define (sum? x) (and (pair? x) (eq? (car x) '+)))

; - The addend is the second item of the sum list:
(define (addend s) (cadr s))

; - The augend is the third item of the sum list:
(define (augend s) (caddr s))

; - A product is a list whose first element is the symbol *:
(define (product? x) (and (pair? x) (eq? (car x) '*)))

; - The multiplier is the second item of the product list:
(define (multiplier p) (cadr p))

; - The multiplicand is the third item of the product list:
(define (multiplicand p) (caddr p))

(define (deriv1 exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum1 (deriv1 (addend exp) var)
                              (deriv1 (augend exp) var)))
        ((product? exp)
         (make-sum1
          (make-product1 (multiplier exp)
                        (deriv1 (multiplicand exp) var))
          (make-product1 (deriv1 (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

; Thus we need only combine these with the algorithm as embodied by deriv in order to have
; a working symbolic-differentiation program. Let us look at some examples of its behaviour:

(deriv1 '(+ x 3) 'x)             ; '(+ 1 0)
(deriv1 '(* x y) 'x)             ; '(+ (* x 0) (* 1 y))
(deriv1 '(* (* x y) (+ x 3)) 'x) ; '(+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))

; The program produces answers that are correct; however, they are unsimplified. It is true
; that

;                  d(xy)
;                  ----- = x * 0 + x * y
;                    dx

; but we would like the program to know that x * 0 = 0, 1 * y = y, and 0 + y = y. The 
; answer for the second example should have been simply y. As the third example shows, this
; becomes a serious issue when the expressions are complex.

; Our difficulty is much like the one we encountered with the rational-number implementation:
; we haven't reduced answers to simplest form. To accomplish the rational-number reduction, 
; we needed to change only the constructors and the selectors of the implmentation. We can
; adopt a similar strategy here. We won't change deriv at all. Instead, we will change make-sum
; so that if both summands are numbers, make-sum will add them and return their sum. Also, if
; one of the summands is 0, then make-sum will return the other summand.

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

; This uses the procedure =number?, which checks whether an expression is equal to a given
; number:

(define (=number? exp num) (and (number? exp) (= exp num)))

; Similarly, we will change make-product to build in the rules that 0 times anything is 0 and
; 1 times anything is the thing itself

(define (make-product a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list '* a1 a2))))

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
        (else
         (error "unknown expression type: DERIV" exp))))

; Here is how this version works on our three examples:

(deriv '(+ x 3) 'x)               ; 1
(deriv '(* x y) 'x)               ; 'y
(deriv '(* (* x y) ( + x 3)) 'x)  ; '(+ (* x y) (* y (+ x 3)))

; Although this is quite an improvement, the third example shows that there is still a 
; long way to go before we get a program that puts expressions into a form that we might
; agree is "simplest". The problem of algebraic simplification is complex because, among other 
; reasons, a form that may be simplest for one purpose may not be for another.

