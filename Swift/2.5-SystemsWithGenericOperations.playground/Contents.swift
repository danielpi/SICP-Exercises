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


func attachTag<V>(_ tag: NumberType, value: V) -> Tagged<V> {
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

func put(_ op: String, _ type: TypeKey, _ item: Any) {
    if let _ = globalSelectorTable[type] {
        globalSelectorTable[type]![op] = item
    } else {
        globalSelectorTable[type] = [op: item]
    }
}

func get(_ op: String, _ type: TypeKey) -> Any? {
    return globalSelectorTable[type]?[op]
}

func applyGeneric(_ op: String, _ args: Tagged<Pair> ...) -> Any {
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
    func tag(_ x: Double) -> Tagged<Double> { return attachTag(.number, value: x) }
    put("add", TypeKey(types: [.number, .number]), { x, y in tag(x + y) })
    put("sub", TypeKey(types: [.number, .number]), { x, y in tag(x - y) })
    put("mul", TypeKey(types: [.number, .number]), { x, y in tag(x * y) })
    put("div", TypeKey(types: [.number, .number]), { x, y in tag(x / y) })
    put("make", TypeKey(types: [.number, .number]), { x in tag(x) })
}

//: Users of the Scheme-number package will create (tagged) ordinary numbers by means of the procedure:

func makeSchemeNumber(_ n: Double) -> Tagged<Double> {
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
    func gcd(_ a: Int, _ b: Int) -> Int {
        if b == 0 {
            return abs(a)
        } else {
            return gcd(b, a % b)
        }
    }
    
    func makeRat(_ n: Int, _ d:Int) -> Pair {
        let g = gcd(n, d)
        if d < 0 {
            return Pair(car: n/g, cdr: -d/g)
        } else {
            return Pair(car: n/g, cdr: d/g)
        }
    }
    
    func numer(_ x: Pair) -> Int {
        return x.car as! Int
    }
    func denom(_ x: Pair) -> Int {
        return x.cdr as! Int
    }
    
    func printRat(_ x: Pair) {
        print("\(numer(x))/\(denom(x))")
    }
    
    func addRat(_ x: Pair, _ y: Pair) -> Pair {
        return makeRat((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
    }
    func subRat(_ x: Pair, _ y: Pair) -> Pair {
        return makeRat((numer(x) * denom(y)) - (numer(y) * denom(x)), denom(x) * denom(y))
    }
    func mulRat(_ x: Pair, _ y: Pair) -> Pair {
        return makeRat(numer(x) * numer(y), denom(x) * denom(y))
    }
    func divRat(_ x: Pair, _ y: Pair) -> Pair {
        return makeRat(numer(x) * denom(y), denom(x) * numer(y))
    }
    func isEqualRat(_ x: Pair, _ y: Pair) -> Bool {
        return (numer(x) * denom(y)) == (numer(y) * denom(x))
    }
    
    // Interface to rest of the system
    func tag(_ x: Pair) -> Tagged<Pair> { return attachTag(.rational, value: x) }
    
    put("add", TypeKey(types: [.rational, .rational]), { x, y in return tag(addRat(x,y)) } )
    put("sub", TypeKey(types: [.rational, .rational]), { x, y in return tag(subRat(x,y)) } )
    put("mul", TypeKey(types: [.rational, .rational]), { x, y in return tag(mulRat(x,y)) } )
    put("div", TypeKey(types: [.rational, .rational]), { x, y in return tag(divRat(x,y)) } )
    put("make", TypeKey(types: [.rational, .rational]), { x, y in return tag(makeRat(x,y)) } )
}


func makeRationalNumber(_ n: Int, d: Int) -> Tagged<Pair> {
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

func square(_ x:Double) -> Double { return x * x }

func installRectangularPackage() {
    // Internal Procedures
    func realPart(_ z: Pair) -> Double { return z.car as! Double }
    func imagPart(_ z: Pair) -> Double { return z.cdr as! Double }
    func magnitude(_ z: Pair) -> Double {
        return pow(square(realPart(z)) + square(imagPart(z)), 0.5)
    }
    func angle(_ z: Pair) -> Double {
        return atan2(imagPart(z), realPart(z))
    }
    
    func tag(_ x: Pair) -> Tagged<Pair> {
        return attachTag(.rectangular, value: x)
    }
    func makeFromRealImag(_ x: Double, y: Double) -> Tagged<Pair> {
        return tag(Pair(car: x, cdr: y))
    }
    func makeFromMagAng(_ r: Double, A: Double) -> Tagged<Pair> {
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
    func magnitude(_ z: Pair) -> Double { return z.car as! Double }
    func angle(_ z: Pair) -> Double { return z.cdr as! Double }
    func realPart(_ z: Pair) -> Double {
        return magnitude(z) * cos(angle(z))
    }
    func imagPart(_ z: Pair) -> Double {
        return magnitude(z) * sin(angle(z))
    }
    
    func tag(_ x: Pair) -> Tagged<Pair> {
        return attachTag(.polar, value: x)
    }
    func makeFromMagAng(_ r: Double, A: Double) -> Tagged<Pair> {
        return tag(Pair(car: r, cdr: A))
    }
    func makeFromRealImag(_ x: Double, y: Double) -> Tagged<Pair> {
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
    func makeFromRealImag(_ x: Double, y: Double) -> Tagged<Pair> {
        if let make = get("makeFromRealImag", TypeKey(types: [.rectangular])) as? (Double, Double) -> Tagged<Pair> {
            return make(x, y)
        } else {
            fatalError("Failed to make from Real Imag")
        }
    }
    
    func makeFromMagAng(_ r: Double, A: Double) -> Tagged<Pair> {
        if let make = get("makeFromMagAng", TypeKey(types: [.polar])) as? (Double, Double) -> Tagged<Pair> {
            return make(r, A)
        } else {
            fatalError("Failed to make from Mag Ang")
        }
    }
    func magnitude(_ z: Pair) -> Double { return z.car as! Double }
    func angle(_ z: Pair) -> Double { return z.cdr as! Double }
    func realPart(_ z: Pair) -> Double {
        return magnitude(z) * cos(angle(z))
    }
    func imagPart(_ z: Pair) -> Double {
        return magnitude(z) * sin(angle(z))
    }
    
    // Internal Procedures
    func addComplex(_ z1: Pair, z2: Pair) -> Tagged<Pair> {
        return makeFromRealImag(realPart(z1) + realPart(z2), y: imagPart(z1) + imagPart(z2))
    }
    
    func subComplex(_ z1: Pair, z2: Pair) -> Tagged<Pair> {
        return makeFromRealImag(realPart(z1) - realPart(z2), y: imagPart(z1) - imagPart(z2))
    }
    
    func mulComplex(_ z1: Pair, z2:Pair) -> Tagged<Pair> {
        return makeFromMagAng(magnitude(z1) * magnitude(z2), A: angle(z1) + angle(z2))
    }
    
    func divComplex(_ z1: Pair, z2: Pair) -> Tagged<Pair> {
        return makeFromMagAng(magnitude(z1) / magnitude(z2), A: angle(z1) - angle(z2))
    }
    
    // Interface to rest of the system
    func tag(_ z: Pair) -> Tagged<Pair> {
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

func makeComplexFromRealImag(_ x: Double, y: Double) -> Tagged<Pair> {
    if let make = get("makeFromRealImag", TypeKey(types: [.complex])) as? (Double, Double) -> Tagged<Pair> {
        return make(x, y)
    } else {
        fatalError("Failed to make a complex number")
    }
}

func makeComplexFromMagAng(_ r: Double, A: Double) -> Tagged<Pair> {
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

func + <T> (lhs: Tagged<T>, rhs: Tagged<T>) -> Tagged<T> {
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

//func addComplexToSchemeNumber(z: Tagged<Pair>, x: Tagged<Pair>) -> Tagged<Pair> {
//    return makeComplexFromRealImag(, y: <#T##Double#>)
//}

//: This technique works, but it is cumbersome. With such a system, the cost of introducing a new type is not just the construction of the package of procedures for that type but also the construction and installation of the procedures that implement the cross-type operations. This can easily be much more code than is needed to define the operations on the type itself. The method also undermines our ability to combine separate packages additively, or at least to limit the extent to which the implementors of the individual packages need to take account of other packages. For instance, in the example above, it seems reasonable that handling mized operations on complex numbers and ordinary numbers should be the responsibility of the complex-number package. Combining rational numbers and complex numbers, however, might be done by the complex package, by the rational package, or by some third package that uses operations extracted from these two packages. Formulating coherent policies on the division of responsibility among packages can be an overwhelming task in designing systems with many packages and many cross-type operations.
//:
//: ## Coercion
//: In the general situation of completely unrelated operations acting on completely unrelated types, implementing explicit cross-type operations, cumbersome though it may be, is the best that one can hope for. Fortunately, we can usually do better by taking advantage of additional structure that may be latent in our type system. Often the different data types are not completely independent, and there may be ways by which objects of one type may be viewed as being of another type. This process is called *coercion*. For example, if we are asked to arithmetically combine an ordinary number with a complex number, we can view the ordinary number as a complex number whose imaginary part is zero. This transforms the problem to that of combining two complex numbers, which can be handled in the ordinary way by the complex-arithmetic packge.
//:
//: in general, we can implement this idea by designing coercion procedures that transform an object of one type into an equivilent object of another type. Here is a typical coercion procedure, which transforms a given ordinary number to a complex number with that real part and zero inaginary part:

func schemeNumberToComplex(_ n: Tagged<Double>) -> Tagged<Pair> {
    return makeComplexFromRealImag(n.value, y: 0)
}

//: We install these coercion procedures in a special coercion table, indexed under the names of the two types:

//putCoercion("number", "complex", schemeNumberToComplex)

//: (We assume that there are put-coercion and get-coercion procedures available for manipulating this table.) Generally some of the slots in the table will be empty, because it is not generally possible to coerce an arbitrary data object of each type into all other types. For example, there is no way to coerce an arbitrary complex number to an ordinary number, so there will be no general complex->scheme-number procedure included in the table.
//:
//: Once the coercion table has been set up, we can handle coercion in a uniform manner by modifying the apply-generic procedure of Section 2.4.3. When asked to apply an operation, we first check whether the operation is defined for the arguments' types, just as before. If so, we dispatch to the procedure found in the operation-and-type table. Otherwise, we try coercion. For simplicity, we consider only the case where there are two arguments. We check the coercion table to see if objects of the first type can be coerced to the second type. If so, we coerce the first argument and try the operation again. If the objects of the first type cannot in general be coerced to the second type, we try the coercion the other way around to see if there is a way to coerce the second argument to the type of the first argument. Finally, if there is no known way to coerce either type to the other type, we give up. Here is the procedure:

// (define apply-generic

//: This coercion scheme has many advantages over the methods of defining explicit cross-type operations, as outlined above. Although we still need to write coercion procedures to relate the types (possibly n^2 procedures for a system with n types), we need to write only one procedure for each pair of types rather than a different procedure for each collection of types and each generic operation. What we are counting on here is the fact that the appropriate transformation between types depends only on the types themselves, not on the operation to be applied. 
//:
//: On the other hand, there may be applications for which our coercion scheme is not general enough. Even when neither of the objects to be combined can be converted to the type of the other it may still be possible to perform the operation by converting both objects to a third type. In order to deal with such complexity and still preserve modularity in our programs, it is usually necessary to build systems that take advantage of still further structure in the relations among types, as we discuss next.

//: ## Hierarchies of types
//: The coercion scheme presented above relied on the existence of natural relations between pairs of types. Often there is more "global" structure in how the different types relate to each other. For instance, suppose we are building a generic arithmetic system to handle integers, rational numbers, real numbers, and complex numbers. In such a system, it is quite natural to regard an integer as a special kind of rational number, which is in turn a special kind of real number, which is in turn a special kind of complex number. What we actually have is a so-called *hierarchy of types*, in which, for example, integers are a *subtype* of rational numbers (i.e., any operation that can be applied to a rational number can automatically be applied to an integer). Conversely, we say that rational numbers for a *supertype* of intergers. The particular hierarchy we have here is of a very simple kind, in which each type has at most one supertype and at most mone subtype. Such a structure, called a *tower*, is illustrated in Figure 2.25.
//:
//:    complex
//:       ^
//:       |
//:      real
//:       ^
//:       |
//:    rational
//:       ^
//:       |
//:    integer
//: 
//: **Figure 2.25:** A tower fo types.
//:
//: If we have a tower structure, then we can greatly simplify the problem of adding a new type to the hierarchy, for we need only specify how the new type is embedded in the next supertyle above in and how it is the supertype of the type below it. For example, if we want to add an integer to a complex number, we need not explicitly define a special coercion procedure integer ->complex. Insteda, we define how an integer can be transformed into a rational number, how a rational number is transformed into a real number, and how a real number is transformed into a complex number. We then allow the system to transform the integer into a complex number through these steps and then add the two complex numbers.
//:
//: We can redesign our apply-generic procedure in the following way: For each type, we need to supply a *raise* procedure, which "raises" objects of that type one level in the tower. Then when the system is required to operate on objects of different types it can successively raise the lower types until all the objects are at the same level in the tower. (Exercise 2.83 and Exercise 2.84 concern the details of implementing such a strategy.)
//;
//: Another advantage of a tower is that we can easily implement the notion that every type "inherits" all operations defined on a supertype. For instance, if we do not supply a special procedure for finding the real part of an integer, we should nevertheless expect that real-part will be defined for integers by virtue of the fact that integers are a subtype of complex numbers. In a tower, we can arrange for this to happen in a uniform way by modifying apply-generic. If the required operation is not directly defined for the type of the object given, we raise the object to its supertype and try again. We thus crawl up the tower, transforming our arguments as we go, until we either find a level at which the desired operation can be performed or hit the top (in which case we give up)
//:
//: Yet another advantage of a tower over a more general hierarchy is that it gives us a simple way to "lower" a data object to the simplest representation. For example, if we add 2 + 3i to 4 - 3i, it would be nice to obtain the answer as the integer 6 rather than as the complex number 6 + 0i. Exercise 2.85 discusses a way to implement such a lowering operation. (The trick is that we need a general way to distinguish those objects that can be lowered, such as 6 + 0i, from those that cannot, such as 6 + 2i.)
//:
//: ## Inadequacies of hierarchies
//: If the data types in our system can be naturally arranged in a tower, this greatly simplifies the problems of dealing with generic operations on different types, as we have seen. Unfortunately, this is usually not the case. Figure 2.26 illustrates a more complex arrangement of mixed types, 
//:
//:                         polygon
//:                        /       \
//:                       /         \
//:                      /           \
//:                     /             \
//:                    /               \
//:                   /               quadrilateral
//:                  /                 /       \
//:                 /                 /         \
//:              triangle       trapezoid        \
//:               /    \              \           \
//:              /      \              \           \
//:          isosceles  right      parallelogram   kite
//:          triangle   triangle      /      \    /
//:           /     \      |     rectangle  rhombus
//:          /       \     |            \   /
//:     equilateral   \  isosceles       \ /
//:       triangle    right triangle    square
//:
//: Figure 2.26: Relations among types of geometric figures
//:
//: this one showing relations among different types of geometric figures. We see that, in general, a type may have more than one subtype. Triangles and quadrilaterals, for instance, are both subtypes of polygons. In addition, a type may have more than one supertype. For example, an isosceles right triangle may be regarded either as an isosceles triangle or as a right triangle. This multiple-supertypes issue is particularly thorny, since it means that there is no unique way to "raise" a type in the hierarchy. Finding the "correct" supertype in which to apply an operation to an object may involve considerable searching through the entire type network on the part of a procedure such as apply-generic. Since there generally are multiple subtypes for a type, there is a similar problem in coercing a value "down" the type hierarchy. Dealing with large numbers of interrelated types while stil preserving modularity in the design of large systems is very difficule, and is an area of much current research.


