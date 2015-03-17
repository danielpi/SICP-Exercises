#lang racket

; Chapter 2 - Building Abstractions with Data

; Chapter 1 was focused on computational processes and the role of procedures in program design. We saw how
; to use primitive data (numbers) and primitive operations (arithmetic operations), how to combine procedures 
; to form compond procedures through composition, continionals adn the use of parameters.

; In chapter two we are going to look at more complex data. Chapter 1 was about building abstractions by
; combining procedures to form compound procedures, we turn in this chapter to another key aspect of any
; programming language: the means it provides for building abstractions by combining data objects to form
; combound data

; Why do we want combound data in a programming language?
; - to elevate the conceptual level at which we can design our programs
; - to increase the modularity of our designs
; - enhance expressive power


; Section 2.1
; Introduction to Data Abstraction
; In sections 1.1.8, we noted that a procedure used as an element in creating a more complex procedure
; could be regarded not only as a collection of particular operations but also as a procedural abstraction.
; So details of how the procedure was implemented could be suppressed. In other words, we could make an abstraction
; that would seperate the way the procedure would be used from the details of how the procedure was implemented. The
; analogous notion for compound data is called data abstraction. Which enables us to isolate how a compound data 
; object is used from the details of how it is constracted from more primitive data objects.

; The basic idea of data abstraction is to structure the programs that are to use compound data objects so
; that they operate on "abstract data." Our programs should use data in such a way as to make no assumptions about
; the data that are not strictly nexessary for performing the task at hand. At the same time a "concrete" data
; representation is defined independent of the programs that use the data. The interface between these two parts
; of our system will be a set of procedures, called selectors and constructors, that implement the abstract data in 
; terms of the concrete representations. 

; 2.1.1 Example: Arithmetic Operations for Rational Numbers
; Pairs, To enable us to implement the concrete level of our data abstraction, our language provides a
; compound stracture called a pair, which can be constructed with the primitive procedure cons. This
; procedure takes two arguments and returns a compound data object that contains the two arguments as
; parts. Given a pair, we can extract the parts using the primitive procedures car and cdr. Thus we can
; use cons, car and cdr as follows

(define x (cons 1 2))
(car x) ; 1
(cdr x) ; 2

; Notice that a pair is a data object that can be given a name and manipulated, just like a primitive data
; object. Moreover, cons can be used to form pairs whose elements are pairs, and so on:

(define x2 (cons 1 2))
(define y (cons 3 4))
(define z (cons x2 y))
(car (car z)) ; 1
(car (car z)) ; 3
z

; Later we will see how this ability to combine pairs means that pairs can be used as general-purpose building
; blocks to create all sorts of complex data structures. The single compound-data primitive pair, implemented by
; the procedures cons, car and cdr, is the only glue we need. Data objects constructed from pairs are called
; list-structured data.

; Representing Rational Numbers
; Pairs offer a natural way to complete the rational number system. 

(define (make-rat1 n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

; Also in order to display the results of our computations, we can print rational numbers by printing the
; numerator, a slash and the denominator

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; From earlier

(define (add-rat1 x y)
  (make-rat1 (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat1 x y)
  (make-rat1 (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat1 x y)
  (make-rat1 (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat1 x y)
  (make-rat1 (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat1? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; Now we can try our ratinal-number procedures:


(define one-half1 (make-rat1 1 2))
(print-rat one-half1) ; 1/2
(define one-third1 (make-rat1 1 3))
(print-rat (add-rat1 one-half1 one-third1)) ; 5/6
(print-rat (mul-rat1 one-half1 one-third1)) ; 1/6
(print-rat (add-rat1 one-third1 one-third1)) ; 6/9

; As the final example shows, our rational number implementation does not reduce rational numbers
; to lowest terms. We can remedy this by changing make-rat. If we use the gcd procedure from section
; 1.2.5, we can reduce the numerator and denominator to lowest terms before construction the pair
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(gcd 5 6)
(gcd 6 9)

(define (make-rat n d) 
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
; (define (numer x) (car x))
; (define (denom x) (cdr x))


(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define one-half (make-rat1 1 2))
(print-rat one-half) ; 1/2
(define one-third (make-rat1 1 3))
(print-rat (add-rat one-half one-third)) ; 5/6
(print-rat (mul-rat one-half one-third)) ; 1/6
(print-rat (add-rat one-third one-third)) ; 6/9


; 2.1.2 Abstraction Barriers
; Before continuing, let us consider some of the issues raised bu the rational-number example we have
; just completed. We defined the rational number operations in terms of a constructor make-rat and
; selectors numer and denom. In general the underlying idea of data abstraction is to identify for each
; type of data object a basic set of operations in terms of which all manipulations of data objects of 
; that type will be expressed, and then to use only those operations in manipulating the data.

; We can envision the structure of the rational-number system as shown below
;       ------------------------------------
;  ----- Programs that use rational numbers -----
;       ------------------------------------
;        Rational numbers in problem domain
;
;               ---------------------
;  ------------- add-rat sub-rat ... ------------
;               ---------------------
;  Rational numbers as numerators and denominators
;
;               ----------------------
;  ------------- make-rat numer denom -----------
;               ----------------------
;           Rational numbers as pairs
;
;                   --------------
;  ----------------- cons car cdr ----------------
;                   --------------
;         However pairs are implemented

; The horizontal lines represent abstraction barriers that isolate different "levels" of the
; system. At each level, the barrier seperates the programs (above) that use the data abstraction
; from the programs (below) that implement the data abstraction. Programs that use rational numbers
; manipulate them solely in terms of the procedures supplied "for public use" by the rational-number
; package: add-rat, sub-rat, mul-rat, div-rat, and equal-rat?. These in turn are implemented solely
; in terms of the constructor and selectors make-rat, numer and denom, which themselves are implemented
; in terms of pairs. The details of how pairs are implemented are irrelevant to the rest of the rational
; number package so long as pairs can be manipulated by the use of cons, car and cdr. In effect, 
; procedures at each level are the interfaces that define the abstraction barriers and connect the
; different levels.

; This simple idea has many advantages. One advantage is that it makes programs much easier to
; maintain and to modify. Any complex data structure can be represented in a variety of ways with the
; primitive data structures provided by a programming language. Of course, the choice of representaion
; influences the programs that operate on it: thus if the representation were to be changed at some
; later time, all such programs might have to be modified accordingly. This taske would be time consuming.

; For example, an alternative way to address the problem of reducing rational numbers to lowest terms is
; to perform the reduction whenever we access the parts of a rational number, rather than when we
; construct it. This leads to different constructor and selector procedures:

(define (make-rat2 n d)
  (cons n d))
(define (numer2 x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))
(define (denom2 x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

; The difference between this implementation and the previous one lies in when we compute the gcd. If
; in our typical use of rational numbers we access the numerators and denominators of the same rational
; numbers many times, it would be preferable to compute the gcd when the rational numbers are constructed.
; If not, we may be better off waiting until access time to compute the gcd. In any case, when we change
; from one prepresentation to the other, the procedures add-rat, sub-rat, and so on do not have to be 
; modified at all.

; Constraining the dependence on the representation to a few interface procedures helps us design
; programs as well as modify them, because it allows us to maintain the flexibility to consider alternate
; implementations. To continue with our simple example, suppose we are designing a rational-number
; package and we can't decide initially whether to perform the gcd at construction time or at selection
; time. The data-abstraction methodology gives us a way to defer that decision without losing the ability
; to make progress on the rest of the system.


; 2.1.3 What is meant by Data
; We began the rational-number implementation by implementing the rational number operations add-rat,
; sub-rat and so on interms of three unspecified procedures: make-rat, numer and denom. At that point we
; could think of the operations as being defined in terms of data objects -- numerators, denominators and
; rational numbers -- whose behaviour was specified by the latter three procedures.

; But exactly what is meant by data? It is not enough to say "Whatever is implemented by the given
; selectors and constructors. We need to guarantee that, if we construct a rational number x from a pair
; of integers n and d, then extracting the numer and denom of x and dividing them should yield the same
; result as dividing n by d.

; In fact this is the only condition make-rat, numer and denom must fulfill in order to form a suitable
; basis for a rational-number representation. In general, we can think of data as defined by some collection
; of selectors and constructors, together with specified conditions that these procedures must fulfill in
; order to be valid representations.

; This point of view can serve to define not only "high-level" data objects but low level objects such
; as pair as well. We never actually said what a pair was, only that the language supplied procedures
; cons, car and cdr for operating on pairs. But the only thing we need to know about these three operations
; is that if we glue two objects together using cons we can retreive the objects using car and cdr. Any 
; triple of procedures that satisfies the above condition can be used as the basis for implementing pairs.

; This point is illustrated strikingly by the fact that we could implement cons, car and cdr without using
; any data structures at all but only using procedures. Here are the definitions

(define (cons2 x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car2 z) (z 0))
(define (cdr2 z) (z 1))

(define (make-rat3 n d) 
  (let ((g (gcd n d)))
    (cons2 (/ n g) (/ d g))))
(define (numer3 x) (car2 x))
(define (denom3 x) (cdr2 x))

(define (print-rat3 x)
  (newline)
  (display (numer3 x))
  (display "/")
  (display (denom3 x)))

(print-rat3 (make-rat3 5 2))

; This use of procedures corresponds to nothing like our intuitive notion of what data should be.
; Nevertheless, all we need to do to show that this is a valid way to repreent pairs is to verify
; that these procedures satisfy the conditions given above.

; The procedural implementation of pairs is a valid implementation, and if we access pairs using
; only cons, car and cdr we cannot distinguish this implementation from one that uses "real" data
; structures.

; The point of exhibiting the procedureal representation of pairs is not that our language works
; this way (in general it does not) but that it could work this way. The procedural representation
; is perfectly adequate for representing pairs since it fulfills the only conditions that pairs need
; to fulfill. 


; 2.1.4 Extended Exercise: Interval Arithmetic
; For an engineering system it is sometimes necessary to have the ability to manipulate inexact
; quantities with known precision, so that when computations are donw with such approximate
; quantities the results will be numbers of known precision.

; Electrical engineers couls use this system to compute electrical quantities. For instance in the
; case of electrical resistance of parallel resistors the formula to use is

;           1
; Rp = -----------
;      1/R1 + 1/R2

; Resistance values are usually known only up to some tolerance guaranteed by the manufacturer of 
; the resistor. For example if you buy a resistor labelled 6.8ohms with 10% tolreance you can only 
; be sure that the resistance is between 6.8 - 0.68 = 6.12 and 6.8 + 0.68 = 7.48 ohms. 

; We can use interval arithmetic as a set of arithmetic operations for combining intervals (objects
; that represent a range of values). The result of adding, subtracting, multiplying or dividing two
; intervals is itself an interval.

; Our interval object will be made of two endpoints: a lower and an upper bound.

(define (make-interval lower upper)
  (cons lower upper))
(define (lower-bound interval)
  (car interval))
(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

; Sometimes it would be better to work with intervals that are specified as centers and widths
; rather than upper and lower bounds.

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; Alternatively we would like to be able to work with intervals that are presented as a center
; and a percentage tolerance. See question 2.12

(define (make-center-percent c t)
  (make-interval (- c (* c t)) (+ c (* c t))))
(define (percent i)
  (/ (width i) (center i)))

; The formula for parallel resistors can be written in two algebraically equivilent ways
;
;  R1 R2
; -------
; R1 + R2
;
;                and
;
;      1
; -----------
; 1/R1 + 1/R2

; Which can be written as

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; These two bits of code give different results for the same input





