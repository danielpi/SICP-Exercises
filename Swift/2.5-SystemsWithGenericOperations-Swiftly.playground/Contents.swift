import Cocoa

// Swift has built in Generics and a powerful type system. This is an attempt to work with those features to achieve the goals of this chapter rather than trying to fight against them.

//: # Systems with Generic Operations
//:
//: In the previous section, we saw how to design systems in which data objects can be represented in more than one way. The key idea is to link the code that specifies the data operations to the several representations by means of generic interface procedures. Now we will see how to use this same idea not only to define operations that are generic over different representations but also to define operations that are generic over different kinds of arguments. We have already seen several different packages of arithmetic operations
//: - the primitive arithmetic (+, -, *, /) built into our language
//: - the rational-number arithmetic (add-rat, sub-rat, mul-rat, div-rat) of Section 2.1.1
//: - and the complex-number arithmetic that we implemented in Section 2.4.3.
//:
//: We will now use data-directed techniques to construct a package of arithmetic operations that incorporates all the arithmetic packages we have already constructed.
//:
//: Figure 2.23 shows the structure of the system we shall build. Notice the abstraction barriers. From the perspective of someone using "numbers", there is a single procedure *add* that operates on whatever numbers are supplied. *add* is part of a generic interface that allows the separate ordinary-arithmetic, rational-arithmetic and complex-arithmetic packages to be accessed uniformly by programs that use numbers. Any individual arithmetic package (such as the complex package) may itself be accessed through generic procedures (such as add-complex) that combine packages designed for different representations (such as rectangular and polar). Moreover, the structure of the system is additive, so that one can design the individual arithmetic packages separately and combine them to produce a generic arithmetic system.
//:
//:
//:                            Programs that use numbers
//:                               +-----------------+
//:     --------------------------| add sub mul div |----------------------
//:                               +-----------------+
//:                             Generic arithmetic package
//:       +-----------------+   +-------------------------+      +-----+
//:     __| add-rat sub-rat |___| add-complex sub-complex |______| + - |___
//:       | mul-rat div-rat | | | mul-complex div-complex | |    | * / |
//:       +-----------------+ | +-------------------------+ |    +-----+
//:                           |     Complex arithmetic      |
//:            Rational       |-----------------------------|   Ordinary
//:           arithmetic      |   Rectangular |    Polar    |  arithmetic
//:     ----------------------+-----------------------------+--------------
//:                 List structure and primitive machine arithmetic
//:
//:  Figure 2.23: Generic arithmetic system
//:
//:
//: ## 2.5.1 Generic Arithmetic Operations
//:
//: The task of designing generic arithmetic operations is analogous to that of designing the generic complex-number operations. We would like, for instance, to have a generic addition procedure *add* that acts like ordinary primitive addition + on ordinary numbers, like add-rat on rational numbers, and like add-complex on complex numbers. We can implement add, and the other generic arithmetic operations, by following the same strategy we used in Section 2.4.3 to implement the generic selectors for complex numbers. We will attach a type tag to each kind of number and cause the generic procedure to dispatch to an appropriate package according to the data type of its arguments.
//:
//: The generic arithmetic procedures are defined as follows:

/*
func add(x: Double, y: Double) -> Double { return applyGeneric("add")(x,y) }
func sub(x: Double, y: Double) -> Double { return applyGeneric("sub")(x,y) }
func mul(x: Double, y: Double) -> Double { return applyGeneric("mul")(x,y) }
func div(x: Double, y: Double) -> Double { return applyGeneric("div")(x,y) }
*/

//: We begin by installing a package for handling *ordinary* numbers, that is, the primitive numbers of our language. We will tag these with the symbol *scheme-number*. The arithmetic operations in this package are the primitive arithmetic procedures (so there is no need to define extra procedures to handle the untagged numbers). Since these operations each take two arguments, they are installed in the table keyed by the list (scheme-number scheme-number):

//: Users of the Scheme-number package will create (tagged) ordinary numbers by means of the procedure:

let a = 9.4


//: Now that the framework of the generic arithmetic system is in place, we can readily include new kinds of numbers. Here is a package that performs rational arithmetic. Notice that, as a benefit of additivity, we can use without modification the rational-number code from Section 2.1.1 as the internal procedures in the package:

struct Rational  {
    let numer: Int
    let denom: Int
    
    init(n: Int, d: Int) {
        let g = Rational.gcd(n, d)
        if d < 0 {
            self.numer = n/g
            self.denom = -d/g
        } else {
            self.numer = n/g
            self.denom = d/g
        }
    }
    
    static func gcd(a: Int, _ b: Int) -> Int {
        if b == 0 {
            return abs(a)
        } else {
            return gcd(b, a % b)
        }
    }
}

extension Rational: CustomStringConvertible {
    var description: String {
        return "\(self.numer)/\(self.denom)"
    }
}


func + (lhs: Rational, rhs: Rational) -> Rational {
    return Rational(n: (lhs.numer * rhs.denom) + (rhs.numer * lhs.denom), d: lhs.denom * rhs.denom)
}
func - (lhs: Rational, rhs: Rational) -> Rational {
    return Rational(n: (lhs.numer * rhs.denom) - (rhs.numer * lhs.denom), d: lhs.denom * rhs.denom)
}
func * (lhs: Rational, rhs: Rational) -> Rational {
    return Rational(n: lhs.numer * rhs.numer, d: lhs.denom * rhs.denom)
}
func / (lhs: Rational, rhs: Rational) -> Rational {
    return Rational(n: lhs.numer * rhs.denom, d: lhs.denom * rhs.numer)
}

extension Rational: Equatable {}
func == (lhs: Rational, rhs: Rational) -> Bool {
    return (lhs.numer * rhs.denom) == (rhs.numer * lhs.denom)
}

let c = Rational(n: 4, d: 8)
let d = c + Rational(n: 3, d: 4)
let e = d - Rational(n: 1, d: 3)
let f = e * c
let g = f / d

//: We can install a similar package to handle complex numbers, using the tag complex. In creating the package, we extract from the table the operations make-from-real-imag and make-from-mag-ang that were defined by the rectangular and polar packages. Additivity permits us to use, as the internal operations, the same add-complex, sub-complex, mul-complex, and div-complex procedures from Section 2.4.1.

struct Complex {
    let real: Double
    let imag: Double
    
    var mag: Double {
        return pow(Complex.square(real) + Complex.square(imag), 0.5)
    }
    var ang: Double {
        return atan2(imag, real)
    }
    
    init(r: Double, i: Double) {
        self.real = r
        self.imag = i
    }
    init(m: Double, A: Double) {
        self.real = m * cos(A)
        self.imag = m * sin(A)
    }
    
    static func square(x: Double) -> Double { return x * x }
}
extension Complex: CustomStringConvertible {
    var description: String {
        return "\(real) + \(imag)i"
    }
}

func + (lhs: Complex, rhs: Complex) -> Complex {
    return Complex(r: lhs.real + rhs.real, i: lhs.imag + rhs.imag)
}
func - (lhs: Complex, rhs: Complex) -> Complex {
    return Complex(r: lhs.real - rhs.real, i: lhs.imag - rhs.imag)
}
func * (lhs: Complex, rhs: Complex) -> Complex {
    return Complex(m: lhs.mag * rhs.mag, A: lhs.ang + rhs.ang)
}
func / (lhs: Complex, rhs: Complex) -> Complex {
    return Complex(m: lhs.mag / rhs.mag, A: lhs.ang - rhs.ang)
}

extension Complex: Equatable {}
func == (lhs: Complex, rhs: Complex) -> Bool {
    return (lhs.real == rhs.real) && (lhs.imag == rhs.imag)
}

let h = Complex(r: 3, i: 4)
h.mag
let i = h + Complex(m: 2, A: 0.3)
let j = i - Complex(r: -1, i: -3)
let k = j * Complex(r: -2, i: 2)
let l = k / Complex(m: 4, A: 0)

//: Programs outside the complex-number package can construct complex numbers either from real and imaginary parts or from magnitudes and angles. Notice how the underlying procedures, originally defined in the rectangular and polar packages, are exported to the complex package, and exported from there to the outside world. Note: Not so much here as I am just using the Swift approach.
//:
//: What we have here is a two-level tag system. A typical complex number, such as 3 + 4i in rectangular form. would be represented as shown in Figure 2.24. The outer tag (complex) is used to direct the number to the complex package. Once within the complex pakage, the next tag (rectangular) is used to direct the number to the rectangular package. In a large and complicated system there might be many levels, each interfaced with the next by means of generic operations. As a data object is passed "downward," the outer tag that is used to direct it to the approptiate package is stripped off (by applying contents) and the next level of tag (if any) becomes visible to be used for further dispatching. 
//:
//: In the above packages, we used add-rat, add-complex, and the other arithmetic procedures exactly as originally written. Once these definitions are internal to different installation procedures, however, they no longer need names that are distinct from each other: we could simply name them add, sub, mul and div in both packages.


//: ## 2.5.2 Combining Data of Different Types
//:
//: We have seen how to define a unified arithmetic system that encompasses ordinary numbers, complex numbers, rational numbers, and any other type of number we might decide to invent, but we have ignorde, an important issue. The operations we have defined so far treat the different data types as being competely independent. Thus, there are separate packages for adding, say, two ordinary numbers, or two complex numbers. What we have not yet considered is the fact that it is meaningful to define operations that cross the type boundaries, such as the addition of a complex number to an ordinary number. We have gone to great pains to introduce barriers between parts of our programs so that they can be developed and understood separately. We would like to introduce the cross-type operations in some carefully controlled way, so that we can support them without seriously violating our module boundaries.
//:
//: One way to handle cross-type operations is to design a different procedure for each possible combination of types for which the operation is valid. For example. We could extend the complex-number package so that it provides a procedure for adding complex numbers to ordinary numbers and installs this in the table using the tag(complex scheme-number):
