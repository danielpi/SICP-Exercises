import Foundation
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

//typealias Tagged = (String, Double)
enum NumberType {
    case number, complex, rational, polar, rectangular
}

struct Tagged<V>: CustomStringConvertible {
    let type: NumberType
    let value: V
    
    var description: String {
        return "\(type):\(value)"
    }
}

struct Pair: CustomStringConvertible {
    let car: Any
    let cdr: Any
    
    var description: String {
        return "(\(car), \(cdr))"
    }
}


func attachTag<V>(tag: NumberType, value: V) -> Tagged<V> {
    return Tagged(type: tag, value: value)
}


// http://stackoverflow.com/questions/24021950/how-do-i-put-different-types-in-a-dictionary-in-the-swift-language

struct TypeKey: Equatable, Hashable {
    let types: [NumberType]
    
    var hashValue: Int {
        return types.reduce(0) { $0 ^ $1.hashValue }
    }
}
func == (lhs: TypeKey, rhs: TypeKey) -> Bool {
    var result = true
    
    if lhs.types.count == rhs.types.count {
        let zipped = zip(lhs.types, rhs.types)
        
        for (l, r) in zipped {
            result = result && (l == r)
        }
    } else {
        result = false
    }
    return result
}

var globalSelectorTable = [TypeKey: [String: Any]]()

func put(op: String, _ type: TypeKey, _ item: Any) {
    if let _ = globalSelectorTable[type] {
        globalSelectorTable[type]![op] = item
    } else {
        globalSelectorTable[type] = [op: item]
    }
}

func get(op: String, _ type: TypeKey) -> Any? {
    return globalSelectorTable[type]?[op]
}

func applyGeneric(op: String, _ args: Tagged<Pair> ...) -> Any {
    let typeTags = TypeKey(types: args.map { $0.type })
    
    if let proc = get(op, typeTags) {
        return proc
    } else {
        fatalError("There is no selector named \(op) for data of type \(typeTags) registered with \(globalSelectorTable)")
    }
}

func add(x: Tagged<Pair>, y: Tagged<Pair>) -> Tagged<Pair> {
    let proc = applyGeneric("add") as! (Tagged<Pair>, Tagged<Pair>) -> Tagged<Pair>
    return proc(x,y)
}
func sub(x: Tagged<Pair>, y: Tagged<Pair>) -> Tagged<Pair> {
    let proc = applyGeneric("sub") as! (Tagged<Pair>, Tagged<Pair>) -> Tagged<Pair>
    return proc(x,y)
}
func mul(x: Tagged<Pair>, y: Tagged<Pair>) -> Tagged<Pair> {
    let proc = applyGeneric("mul") as! (Tagged<Pair>, Tagged<Pair>) -> Tagged<Pair>
    return proc(x,y)
}
func div(x: Tagged<Pair>, y: Tagged<Pair>) -> Tagged<Pair> {
    let proc = applyGeneric("div") as! (Tagged<Pair>, Tagged<Pair>) -> Tagged<Pair>
    return proc(x,y)
}

func installSwiftNumberPackage() {
    func tag(x: Double) -> Tagged<Double> { return attachTag(.number, value: x) }
    put("add", TypeKey(types: [.number, .number]), { x, y in tag(x + y) })
    put("sub", TypeKey(types: [.number, .number]), { x, y in tag(x - y) })
    put("mul", TypeKey(types: [.number, .number]), { x, y in tag(x * y) })
    put("div", TypeKey(types: [.number, .number]), { x, y in tag(x / y) })
    put("make", TypeKey(types: [.number, .number]), { x in tag(x) })
}

//: Users of the Scheme-number package will create (tagged) ordinary numbers by means of the procedure:

func makeSchemeNumber(n: Double) -> Tagged<Double> {
    if let make = get("make", TypeKey(types: [.number, .number])) as? (Double) -> Tagged<Double> {
        return make(n)
    } else {
        fatalError("makeSchemeNumber hasn't been implemented")
    }
}

installSwiftNumberPackage()
let a = makeSchemeNumber(9.4)
let b = makeSchemeNumber(12.8)


//: Now that the framework of the generic arithmetic system is in place, we can readily include new kinds of numbers. Here is a package that performs rational arithmetic. Notice that, as a benefit of additivity, we can use without modification the rational-number code from Section 2.1.1 as the internal procedures in the package:

// This doesn't work at the moment, but I am getting bogged down.

func installRationalPackage() {
    // Internal Procedures from Ex 2.1.1
    func gcd(a: Int, _ b: Int) -> Int {
        if b == 0 {
            return abs(a)
        } else {
            return gcd(b, a % b)
        }
    }
    
    func makeRat(n: Int, _ d:Int) -> Pair {
        let g = gcd(n, d)
        if d < 0 {
            return Pair(car: n/g, cdr: -d/g)
        } else {
            return Pair(car: n/g, cdr: d/g)
        }
    }
    
    func numer(x: Pair) -> Int {
        return x.car as! Int
    }
    func denom(x: Pair) -> Int {
        return x.cdr as! Int
    }
    
    func printRat(x: Pair) {
        print("\(numer(x))/\(denom(x))")
    }
    
    func addRat(x: Pair, _ y: Pair) -> Pair {
        return makeRat((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
    }
    func subRat(x: Pair, _ y: Pair) -> Pair {
        return makeRat((numer(x) * denom(y)) - (numer(y) * denom(x)), denom(x) * denom(y))
    }
    func mulRat(x: Pair, _ y: Pair) -> Pair {
        return makeRat(numer(x) * numer(y), denom(x) * denom(y))
    }
    func divRat(x: Pair, _ y: Pair) -> Pair {
        return makeRat(numer(x) * denom(y), denom(x) * numer(y))
    }
    func isEqualRat(x: Pair, _ y: Pair) -> Bool {
        return (numer(x) * denom(y)) == (numer(y) * denom(x))
    }
    
    // Interface to rest of the system
    func tag(x: Pair) -> Tagged<Pair> { return attachTag(.rational, value: x) }
    
    put("add", TypeKey(types: [.rational, .rational]), { x, y in return tag(addRat(x,y)) } )
    put("sub", TypeKey(types: [.rational, .rational]), { x, y in return tag(subRat(x,y)) } )
    put("mul", TypeKey(types: [.rational, .rational]), { x, y in return tag(mulRat(x,y)) } )
    put("div", TypeKey(types: [.rational, .rational]), { x, y in return tag(divRat(x,y)) } )
    put("make", TypeKey(types: [.rational, .rational]), { x, y in return tag(makeRat(x,y)) } )
}


func makeRationalNumber(n: Int, d: Int) -> Tagged<Pair> {
    if let make = get("make", TypeKey(types: [.rational, .rational])) as? (Int, Int) -> Tagged<Pair> {
        return make(n, d)
    } else {
        fatalError("makeSchemeNumber hasn't been implemented")
    }
}

installRationalPackage()
let c = makeRationalNumber(3, d: 4)
let d = makeRationalNumber(2, d: 5)


//: We can install a similar package to handle complex numbers, using the tag complex. In creating the package, we extract from the table the operations make-from-real-imag and make-from-mag-ang that were defined by the rectangular and polar packages. Additivity permits us to use, as the internal operations, the same add-complex, sub-complex, mul-complex, and div-complex procedures from Section 2.4.1.

func square(x:Double) -> Double { return x * x }

func installRectangularPackage() {
    // Internal Procedures
    func realPart(z: Pair) -> Double { return z.car as! Double }
    func imagPart(z: Pair) -> Double { return z.cdr as! Double }
    func magnitude(z: Pair) -> Double {
        return pow(square(realPart(z)) + square(imagPart(z)), 0.5)
    }
    func angle(z: Pair) -> Double {
        return atan2(imagPart(z), realPart(z))
    }
    
    func tag(x: Pair) -> Tagged<Pair> {
        return attachTag(.rectangular, value: x)
    }
    func makeFromRealImag(x: Double, y: Double) -> Tagged<Pair> {
        return tag(Pair(car: x, cdr: y))
    }
    func makeFromMagAng(r: Double, A: Double) -> Tagged<Pair> {
        return tag(Pair(car:r * cos(A), cdr:r * sin(A)))
    }
    
    // Interface to the rest of the system
    
    put("realPart", TypeKey(types:[.rectangular]), realPart)
    put("imagPart", TypeKey(types:[.rectangular]), imagPart)
    put("magnitude", TypeKey(types:[.rectangular]), magnitude)
    put("angle", TypeKey(types:[.rectangular]), angle)
    put("makeFromRealImag", TypeKey(types:[.rectangular]), makeFromRealImag)
    put("makeFromMagAng", TypeKey(types:[.rectangular]), makeFromMagAng)
}
installRectangularPackage()

func installPolarPackage() {
    // Internal Procedures
    func magnitude(z: Pair) -> Double { return z.car as! Double }
    func angle(z: Pair) -> Double { return z.cdr as! Double }
    func realPart(z: Pair) -> Double {
        return magnitude(z) * cos(angle(z))
    }
    func imagPart(z: Pair) -> Double {
        return magnitude(z) * sin(angle(z))
    }
    
    func tag(x: Pair) -> Tagged<Pair> {
        return attachTag(.polar, value: x)
    }
    func makeFromMagAng(r: Double, A: Double) -> Tagged<Pair> {
        return tag(Pair(car: r, cdr: A))
    }
    func makeFromRealImag(x: Double, y: Double) -> Tagged<Pair> {
        return tag(Pair(car: pow(square(x) + square(y), 0.5), cdr:atan2(y, x)))
    }
    
    // interface to the rest of the system
    put("magnitude", TypeKey(types: [.polar]), magnitude)
    put("angle", TypeKey(types: [.polar]), angle)
    put("realPart", TypeKey(types: [.polar]), realPart)
    put("imagPart", TypeKey(types: [.polar]), imagPart)
    put("makeFromMagAng", TypeKey(types: [.polar]), makeFromMagAng)
    put("makeFromRealImag", TypeKey(types: [.polar]), makeFromMagAng)
}
installPolarPackage()

func installComplexPackage() {
    // Imported procedures from rectangular and polar packages
    func makeFromRealImag(x: Double, y: Double) -> Tagged<Pair> {
        if let make = get("makeFromRealImag", TypeKey(types: [.rectangular])) as? (Double, Double) -> Tagged<Pair> {
            return make(x, y)
        } else {
            fatalError("Failed to make from Real Imag")
        }
    }
    
    func makeFromMagAng(r: Double, A: Double) -> Tagged<Pair> {
        if let make = get("makeFromMagAng", TypeKey(types: [.polar])) as? (Double, Double) -> Tagged<Pair> {
            return make(r, A)
        } else {
            fatalError("Failed to make from Mag Ang")
        }
    }
    func magnitude(z: Pair) -> Double { return z.car as! Double }
    func angle(z: Pair) -> Double { return z.cdr as! Double }
    func realPart(z: Pair) -> Double {
        return magnitude(z) * cos(angle(z))
    }
    func imagPart(z: Pair) -> Double {
        return magnitude(z) * sin(angle(z))
    }
    
    // Internal Procedures
    func addComplex(z1: Pair, z2: Pair) -> Tagged<Pair> {
        return makeFromRealImag(realPart(z1) + realPart(z2), y: imagPart(z1) + imagPart(z2))
    }
    
    func subComplex(z1: Pair, z2: Pair) -> Tagged<Pair> {
        return makeFromRealImag(realPart(z1) - realPart(z2), y: imagPart(z1) - imagPart(z2))
    }
    
    func mulComplex(z1: Pair, z2:Pair) -> Tagged<Pair> {
        return makeFromMagAng(magnitude(z1) * magnitude(z2), A: angle(z1) + angle(z2))
    }
    
    func divComplex(z1: Pair, z2: Pair) -> Tagged<Pair> {
        return makeFromMagAng(magnitude(z1) / magnitude(z2), A: angle(z1) - angle(z2))
    }
    
    // Interface to rest of the system
    func tag(z: Pair) -> Tagged<Pair> {
        return attachTag(.complex, value: z)
    }
    
    put("add", TypeKey(types: [.complex,.complex]), addComplex)
    put("sub", TypeKey(types: [.complex,.complex]), subComplex)
    put("mul", TypeKey(types: [.complex,.complex]), mulComplex)
    put("div", TypeKey(types: [.complex,.complex]), divComplex)
    put("makeFromRealImag", TypeKey(types: [.complex,.complex]), makeFromRealImag)
    put("makeFromMagAng", TypeKey(types: [.complex,.complex]), makeFromMagAng)
}
installComplexPackage()

//: Programs outside the complex-number package can construct complex numbers either from real and imaginary parts or from magnitudes and angles. Notice how the underlying procedures, originally defined in the rectangular and polar packages, are exported to the complex package, and exported from there to the outside world.

func makeComplexFromRealImag(x: Double, y: Double) -> Tagged<Pair> {
    if let make = get("makeFromRealImag", TypeKey(types: [.complex])) as? (Double, Double) -> Tagged<Pair> {
        return make(x, y)
    } else {
        fatalError("Failed to make a complex number")
    }
}

func makeComplexFromMagAng(r: Double, A: Double) -> Tagged<Pair> {
    if let make = get("makeFromMagAng", TypeKey(types: [.complex])) as? (Double, Double) -> Tagged<Pair> {
        return make(r, A)
    } else {
        fatalError("Failed to make a complex number from mag ang")
    }
}

//: What we have here is a two-level tag system. A typical complex number, such as 3 + 4i in rectangular form, would be represented as shown in Figure 2.24.
//:
//: Complex, Rectangular, 3, 4
//: Figure 2.24 Representation of 3 + 4i in rectangular form.
//:
//: The outer tag (complex) is used to direct the number to the complex pachage. Once within the complex package, the next tag (rectangular) is used to direct the number to the rectangular package. In a large and complicated system there might be many levels, each interfaced with the next by means of generic operations. As a data object is passed "downward," the outer tag that is used to direct it to the appopriate package is stripped off (by applying contents) and the next level of tag (if any) becomes visible to be used for further dispatching.
//:
//: In the above packages, we used add-rat, add-complex, and the other arithmetic procedures exactly as originally written. Once these definitions are internal to different installation procedures, however, they no longer need names that are distinct from each other: we could simply name them add, sub, mul, and div in both packages.

func +<T> (lhs: Tagged<T>, rhs: Tagged<T>) -> Tagged<T> {
    if let add = get("add", TypeKey(types: [lhs.type, rhs.type])) as? (T, T) -> Tagged<T> {
        return add(lhs.value, rhs.value)
    } else {
        fatalError("Addition is not installed for types: \(TypeKey(types: [lhs.type, rhs.type])))")
    }
}

(a + b).value


//: ## 2.5.2 Combining Data of Different Types
//:
//: We have seen how to define a unified arithmetic system that encompasses ordinary numbers, complex numbers, rational numbers, and any other type of number we might decide to invent, but we have ignorde, an important issue. The operations we have defined so far treat the different data types as being competely independent. Thus, there are separate packages for adding, say, two ordinary numbers, or two complex numbers. What we have not yet considered is the fact that it is meaningful to define operations that cross the type boundaries, such as the addition of a complex number to an ordinary number. We have gone to great pains to introduce barriers between parts of our programs so that they can be developed and understood separately. We would like to introduce the cross-type operations in some carefully controlled way, so that we can support them without seriously violating our module boundaries.
//:
//: One way to handle cross-type operations is to design a different procedure for each possible combination of types for which the operation is valid. For example. We could extend the complex-number package so that it provides a procedure for adding complex numbers to ordinary numbers and installs this in the table using the tag(complex scheme-number):
