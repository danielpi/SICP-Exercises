#lang racket

; Systems with Generic Operations

; In the previous section, we saw how to design systems in which data objects
; can be represented in more than one way. The key idea is to link the code
; that specifies the data operations to the several representations by means
; of generic interface procedures. Now we will see how to use this same idea
; not only to define operations that are generic over different representations
; but also to define operations that are generic over different kinds of
; arguments. We have already seen several different packages of arithmetic
; operations: the primitive arithmetic (+, -, *, /) built into our language,
; the rational-number arithmetic (add-rat, sub-rat, mul-rat, div-rat) of
; section 2.1.1, and the complec-number arithmetic that we implemented in
; section 2.4.3. We will now use data-directed techniques to construct a
; package of arithmetic operations that incorporates all the arithmetic
; packages we have already constructed.
;
; Figure 2.23 shows the structure of the system we shal build. Notice the
; abstraction barriers. From the perspective of someone using "numbers", there
; is a single procedure add that operates on whatever numbers are supplied.
; add is part of a generic interface that allows the separate ordinary-arithmetic,
; rational-arithmetic, and complex-arithmetic packages to be accessed uniformly
; by programs that use numbers. Any individual arithmetic package (such as
; the complex package) may itself be accessed through generic procedures (such
; as add-complex) that combine packages designed for different representations
; (such as rectangular and polar). Moreover, the structure of the system is
; additive, so that one can design the individual arithmetic packages separately
; and combine them to produce a generic arithmetic system.

;                        Programs that use numbers
;                           +-----------------+
; --------------------------| add sub mul div |----------------------
;                           +-----------------+
;                         Generic arithmetic package
;   +-----------------+   +-------------------------+   +-----+
; __| add-rat sub-rat |___| add-complex sub-complex |___| + - |______
;   | mul-rat div-rat | | | mul-complex div-complex | | | * / |
;   +-----------------+ | +-------------------------+ | +-----+
;                       |     Complex arithmetic      |
;        Rational       |-----------------------------|   Ordinary
;       arithmetic      |   Rectangular |    Polar    |  arithmetic
; ----------------------+-----------------------------+--------------
;             List structure and primitive machine arithmetic
;
;  Figure 2.23: Generic arithmetic system

; 2.5.1 Generic Arithmetic Operations
; The task of designing generic arithmetic operations is analogous to that
; of designing the generic complex-number operations. We would like, for
; instance, to have a generic addition procedure add that acts like ordinary
; primitive addition + on ordinary numbers, like add-rat on rational numbers,
; and like add-complex on complex numbers. We can implement add, and the
; other generic arithmetic operations, by following the same strategy we
; used in Section 2.4.3 to implement generic selectors for complex numbers.
; We will attach a type tag to each kind of number and cause the generic
; procedure to dispatch to an appropriate package according to the data
; type of its arguments.

; The generic arithmetic procedures are defined as follows:

;(define (add x y) (apply-generic 'add x y))
;(define (sub x y) (apply-generic 'sub x y))
;(define (mul x y) (apply-generic 'mul x y))
;(define (div x y) (apply-generic 'div x y))

; We begin by installing a package for handling ordinary numbers, that is,
; the primitive numbers of our language. We will tag these with the symbol
; scheme-number. The arithmetic operations in this package are the primitive
; arithmetic procedures (so there is no need to define extra procedures to
; handle the untagged numbers). Since these operations each take two arguments,
; they are installed in the table keyed by the list
; (scheme-number scheme-number):


