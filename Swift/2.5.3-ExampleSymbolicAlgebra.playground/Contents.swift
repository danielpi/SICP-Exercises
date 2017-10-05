import Cocoa

//: # Example: Symbolic Algebra
//:
//: The manipulation of symbolic algebraic expressions is a complex process that illustrates many of the hardest problems that occur in the design of large-scale systems. An algebraic expression in general, can be viewed as a heirachical structure, a tree of operatios applied to operands. We can construct algebraic expressions by starting with a set of primitive opbects, such as constants and variablec, and combining these by means of algebraic operators, such as addition and multiplication. As in other languages, we form abstractions that enable us to reger to compound objects in simple terms. Typical abstractions in symbolic algebra are ideas such as linear combination, polynomial, rational function, or trigonometric function. We can regard these as compound "types," which war often useful for directing the processing of expressions. For example, we could describe the expression
//:
//:     x^2sin
//:
//: as a polynomial in x with coefficients that are trigonometric functions of polynomials in y whose coefficients are integers.
//:
//: We will not attempt to develop a complete algebraic-manipulation system here. Such systems are exceedingly complex programs, embodying deep algebraic knowledge and elegant algorithms. What we will do is look at a simple but important part of algebraic manipulation: the arithmetic of polynomials. We will illustrate the kinds of decisions the designer of such a system faces, and how to apply the ideas of abstract data and generic operations to help organize this effort.
//:
//: ## Arithmetic on polynomials
//:
//: Our first task in designing a system for performing arithmetic on polynomials is to decide just what a polynomial is. Polynomials are normally defined relative to certain variables (the indeterminates of the polynomial). For simplicity, we will restrict ourselves to polynomials having just one indeterminate (univariate polynomials). We will define a polynomial to be a sum of terms, each of which is either a coefficient, a power of the indeterminate, or a product of a coefficient and a power of the indeterminate. A coefficient is defined as an algebraic expression that is not dependent upon the indeterminate of the polynomial. For example,
