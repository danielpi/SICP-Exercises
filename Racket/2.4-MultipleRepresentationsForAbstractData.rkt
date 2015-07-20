#lang racket

; 2.4 Multiple Representations for Abstract Data
;
; We have introduced data abstraction, a methodology for structuring systems in such a way 
; that much of a program can be specified independent of the choices involved in implementing 
; the data objects that the program manipulates. For example, we saw in Section 2.1.1 how to 
; separate the task of designing a program that uses rational numbers from the task of implementing
; rational numbers in terms of the computer language's primitive mechanisms for constructing
; compound data. The key idea was to erect an abstraction barrier - in this case, the selectors
; and constructors for rational numbers (make-rat, numer, denom) - that isolates the way rational 
; numbers are used from their underlying representation in terms of list structure. A similar
; abstraction barrier isolates the details of the procedures that perform rational arithmetic
; (add-rat, sub-rat, mul-rat, and div-rat) from the "higher-level" procedures that use rational 
; numbers. The resulting program has the structure shown in Figure 2.1
;
; These data-abstraction barriers are powerful tools for controlling complexity. By isolating
; the underlying representations of data objects, we can divide the task of designing a large
; program into smaller tasks that can be performed separately. But this kind of data abstraction
; is not yet powerful enough, because it may not always make sense to speak of "the underlying 
; representation" for a data object.
;
; For one thing, there might be more than one useful representation for a data object, and we
; might like to design systems that can deal with multiple representations. To take a simple
; example, complex numbers may be represented in two almost equivalent ways: in rectangular
; form (real and imaginary parts) and in polar form (magnitude and angle). Sometimes rectangular
; form is more appropriate and sometimes polar form is more appropriate. Indeed, it is perfectly
; plausible to imagine a system in which complex numbers are represented in both ways, and in 
; which the procedures for manipulating comples numbers work with either representation.
;
; More importantly, programming systems are often designed by many people working over extended 
; periods of time, subject to requirements that change over time. In such an environment, it is
; simply not possible for everyone to agree in advance on choices of data representation. So in
; addition to the data-abstraction barriers that isolate representation from use, we need abstraction
; barriers that isolate different design choices from each other and permit different choices to
; coexist in a single program. Furthermore, since large programs are often created by combining 
; pre-existing modules that were designed in isolation, we need conventions that permit programmers
; to incoporate modules into larger systems additively, that is, without having to redesign or
; reimplement these modules.
;
; In this section, we will learn how to cope with data that may be represented in different ways
; by different parts of a program. This requires constructing generic procedures - procedures that
; can operate on data that may be represented in more than one way. Our main technique for building
; generic procedures will be to work in terms of data objects that have type tags, that is, data
; objects that include explicit information about how they are to be processed. We will also
; discuss data-directed programming, a powerful and convenient implementation strategy for additively
; assembling systems with generic operations.
; 
; We begin with the simple complex-number example. We will see how type tages and data-directed style
; enable us to design separate rectangular and polar representations for complex numbers while 
; maintaining the notion of an abstract "complex-number" data object. We will accomplish this by
; defining arithmetic procedures for complex numbers (add-complex, sub-complex, mul-complex, and 
; div-complex) in terms of generic selectors that access parts of a complex number independent of
; how the number is represented. The resulting complex-number system, as shown in Figure 2.19,
; contains two different kinds of abstraction barriers. The "horizontal" abstraction barriers
; play the same rols as the ones in Figure 2.1. They isolate "higher-level" operations from
; "lower-level" representations. In addition, there is a "vertical" barrier that gives us the
; ability to separately design and install alternative representations.
;
; In section 2.5 we will show how to use type tags and data-directed style to develop a generic 
; arithmetic package. This provides procedures (add, mul, and so on) that can be used to manipulate 
; all sorts of "numbers" and can be easily extended when a new kind of number is needed. In Section
; 2.5.3, we'll show how to use generic arithmetic in a system that performs symbolic algebra.

;
;            Programs that use complex numbers
;    ----------------------------------------------------
; ---| add-complex sub-complex mul-complex div-complex  |---
;    ----------------------------------------------------
;                Complex-arithmetic package
; ----------------------------------------------------------
;             Rectangular    |       Polar
;           representation   |   representation
; ----------------------------------------------------------
;       List structure and primitive machine arithmetic
;

; Figure 2.19: Data-abstraction barriers in the complex number system.


; 2.4.1 Representations for Complex Numbers
; We will develop a system that performs arithmetic operations on complex numbers as a simple but
; unrealistic example of a program that uses generic operations. We begin by discussing two
; plausible representations for complex numbers as ordered pairs: rectangular form (real part
; and imaginary part) and polar form (magnitude and angle). Section 2.4.2 will show how both 
; representations can be made to coexist in a single system through the use of type tags and
; generic operations.
;

; Imaginary
;     ^
;     |
;     |
;   y |--------+ z = x + iy = re^iA
;     |       /|
;     |      / |
;     |     /  |
;     |  r /   |
;     |   /    |
;     |  /     |
;     | /      |
;     |/ A     |
;   --+--------------->   Real
;     |        x
;
; Figure 2.20: Complex numbers as points in the plane

; Like rational numbers, complex numbers are naturally represented as ordered pairs. The set
; of complex numbers can be thought of as a two-dimensional space with two orthogonal axes, the
; "real" axis and the "imaginary" axis (see figure 2.20.) From this point of view, the complex 
; number z = x + y (where i^2 = -1) can be thought of as the point in the plane whose real coordinate
; is x and whose imaginary coordinate is y. Addition of complex numbers reduces in this representation
; to addition of coordinates:

;      Real-part(z1 + z2) = Real-part(z1) + Real-part(z2),
; Imaginary-part(z1 + z2) = Imaginary-part(z1) + Imaginary-part(z2)

; When multiplying complex numbers, it is more natural to think in terms of representing a complex
; number in polar form, as a magnitude and an angle(r and A in Figure 2.20). The product of two
; complex numbers is the vector obtained by stretching one complex number byt the length of the
; other and then rotating it through the angle of the other:

; Magnitude(z1 * z2) = Magnitude(z1) * Magnitude(z2)
;     Angle(z1 * z2) = Angle(z1_ + Angle(z2)

; Thus, there are two different representations for complex numbers, which are appropriate for
; different operations. Yet, from the fiewpoint of someone writing a program that uses complex
; numbers, the principle of data abstraction suggests that all the operations for manipulating
; complex numbers should be available regardless of which representation is used by the computer.
; For example, it is often useful to be able to find the magnitude of a complex number that is
; specified by rectangular coordinates. Similarly, it is often useful to be able to determine the 
; real part of a complex number that is specified by polar coordinates.

; To design such a system, we can follow the same data-abstraction strategy we followed in
; designing the rational-number package in Section 2.1.1. Assume that the operations on complex
; numbers are implemented in terms of four selectors: real-part, image-part, magnitude and angle.
; Also assume that we have two procedures for constructing complex numbers: make-from-real-image \
; returns a complex number with specified real and imaginary parts, and make-from-mag-ang returns
; a complex number with specified magnitude and angle. These procedures have the property that, 
; for any complex number z, both

; (make-from-real-imag (real-part z) (imag-part z))

; and

; (make-from-mag-ang (magnitude z) (angle z))

; produce complex numbers that are equal to z.

; Using these constructors and selectors, we can implement arithmetic on complex numbers using the 
; "abstract data" specified by the constructors and selectors, just as we did for rational numbers
; in Section 2.1.1. As shown in the formulas above, we can add and subtract complex numbers in
; terms of real and imaginary parts while multiplying and dividing complex numbers in terms of
; magnitudes and angles:

(define (square x) (* x x))

(define (real-part1 z) (car z))
(define (imag-part1 z) (cdr z))
(define (magnitude1 z)
  (sqrt (+ (square (real-part1 z))
           (square (imag-part1 z)))))
(define (angle1 z)
  (atan (imag-part1 z) (real-part1 z)))
(define (make-from-real-imag1 x y) (cons x y))
(define (make-from-mag-ang1 r a)
  (cons (* r (cos a)) (* r (sin a))))


(define (add-complex1 z1 z2)
  (make-from-real-imag1 (+ (real-part1 z1) (real-part1 z2))
                        (+ (imag-part1 z1) (imag-part1 z2))))
(define (sub-complex1 z1 z2)
  (make-from-real-imag1 (- (real-part1 z1) (real-part1 z2))
                        (- (imag-part1 z1) (imag-part1 z2))))
(define (mul-complex1 z1 z2)
  (make-from-mag-ang1 (* (magnitude1 z1) (magnitude1 z2))
                      (+ (angle1 z1) (angle1 z2))))
(define (div-complex1 z1 z2)
  (make-from-mag-ang1 (/ (magnitude1 z1) (magnitude1 z2))
                      (- (angle1 z1) (angle1 z2))))

; To complete the complex-number package, we must choose a representation and we must implement
; the constructors and selectors in terms of primitive numbers and primitive list structure. There
; are two obvious ways to do this: We can represent a complex number in "rectangular form" as a
; pair (real part, imaginary part) or in "polar form" as a pair (magnitude, angle). Which shall
; we choose?

; In order to make the different choices concrete, imagine that there are two programmers, Ben
; Bitdiddle and Alyssa P. Hacker, who are independently designing representations for the complex-
; number system. Ben chooses to represent complex numbers in rectangular form. With this choice,
; selecting the real and imaginary parts of a complex number is straightforward, as is constructing
; a complex number with given real and imaginary parts. To find the magnitude and the angle, or
; to construct a complex number with a given magnitude and angle, he uses the trigonometric
; relations

;    x = r cos A,     r = (x^2 + y^2)^0.5
;    y = r sin A,     A = arctan(y,x)

; which relate the real and imaginary parts (x,y) to the magnitude and the angle (r,A). Ben's
; representation is therefore given by the following selectors and constructors: Shown above

; Alyssa, in contrast, chooses to represent complex numbers in polar form. For her, selecting the
; magnitude and angle is straightforward, but she has to use the trigonometric relations to
; obtain the real and imaginary parts. Alyssa's representation is:

(define (real-part2 z) (* (magnitude2 z) (cos (angle2 z))))
(define (imag-part2 z) (* (magnitude2 z) (sin (angle2 z))))
(define (magnitude2 z) (car z))
(define (angle2 z) (cdr z))
(define (make-from-real-imag2 x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define (make-from-mag-ang2 r a) (cons r a))

; The discipline of data abstraction ensures that the same implementation of add-complex, 
; sub-complex, mul-complex, and div-complex will work with either Ben's representation or
; Alyssa's representation.


; 2.4.2 Tagged data
; One way to view data abstraction is as an application of the "princible or least commitment."
; In implementing the complex-number system in Section 2.4.1, we can use either Ben's rectangular 
; representation or Alyssa's polar representation. The abstraction barrier formed by the selectors 
; and constructors permits us to defer to the last possible moment the choice of a concrete
; representation for our data objects and thus retain maximum flexibility in our system design.

; The principle of least commitment can be carried to even further extremes. If we desire, we
; can maintain the ambiguity of representation even after we have designed the selectors and
; constructors, and elect to use both Ben's representation and Alyssa's representation. If both
; representations are included in a single system, however, we will need some way to distinguish
; data in polar form from data in rectangular form. Otherwise, if we were asked, for instance, to
; find the magnitude of the pair (3, 4), we wouldn't know whether to answer 5 (interpreting the 
; number in rectangular form) or 3 (interpreting the number in polar form). A straightforward
; way to accomplish this distinction is to include a type tag - the symbol rectangular or polar
; - as part of each complex number. Then when we need to manipulate a complex number we can use
; the tag to decide which selector to apply.




