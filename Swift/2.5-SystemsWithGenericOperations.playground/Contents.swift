import Cocoa
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
enum Type {
    case number, complex, rational, polar, rectangular
}

struct Tagged<V> {
    let key: String
    let value: V
}

func attachTag<V>(tag: String, value: V) -> Tagged<V> {
    return Tagged(key: tag, value: value)
}



struct TypeKey: Equatable, Hashable {
    let types: [String]
    
    var hashValue: Int {
        return types.reduce("", combine: +).hashValue
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



func installSwiftNumberPackage() {
    func tag(x: Double) -> Tagged<Double> { return attachTag("swift-number", value: x) }
    put("add", TypeKey(types: ["swift-number", "swift-number"]), { x, y in tag(x + y) })
    put("sub", TypeKey(types: ["swift-number", "swift-number"]), { x, y in tag(x - y) })
    put("mul", TypeKey(types: ["swift-number", "swift-number"]), { x, y in tag(x * y) })
    put("div", TypeKey(types: ["swift-number", "swift-number"]), { x, y in tag(x / y) })
    put("make", TypeKey(types: ["swift-number", "swift-number"]), { x in tag(x) })
}

//: Users of the Scheme-number package will create (tagged) ordinary numbers by means of the procedure:

func makeSchemeNumber(n: Double) -> Tagged<Double> {
    if let make = get("make", TypeKey(types: ["swift-number", "swift-number"])) as? (Double) -> Tagged<Double> {
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

enum ConsPosition {
    case Left, Right
}

typealias Rational = (ConsPosition -> Int)

func installRationalPackage() {
    // Internal Procedures from Ex 2.1.1
    
    
    func cons<T>(a: T, _ b: T) -> (ConsPosition -> T) {
        func innerCons(i: ConsPosition) -> T {
            if i == .Left {
                return a;
            } else {
                return b;
            }
        }
        
        return innerCons;
    }
    
    func car<T>(innerCons: ConsPosition -> T) -> T {
        return innerCons(.Left);
    }
    
    func cdr<T>(innerCons: ConsPosition -> T) -> T {
        return innerCons(.Right);
    }
    
    
    
    func gcd(a: Int, _ b: Int) -> Int {
        if b == 0 {
            return abs(a)
        } else {
            return gcd(b, a % b)
        }
    }
    
    func makeRat(n: Int, _ d:Int) -> Rational {
        let g = gcd(n, d)
        if d < 0 {
            return cons(n/g, -d/g)
        } else {
            return cons(n/g, d/g)
        }
    }
    
    func numer(x: Rational) -> Int {
        return car(x)
    }
    func denom(x: Rational) -> Int {
        return cdr(x)
    }
    
    func printRat(x: Rational) {
        print("\(numer(x))/\(denom(x))")
    }
    
    func addRat(x: Rational, _ y: Rational) -> Rational {
        return makeRat((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
    }
    func subRat(x: Rational, _ y: Rational) -> Rational {
        return makeRat((numer(x) * denom(y)) - (numer(y) * denom(x)), denom(x) * denom(y))
    }
    func mulRat(x: Rational, _ y: Rational) -> Rational {
        return makeRat(numer(x) * numer(y), denom(x) * denom(y))
    }
    func divRat(x: Rational, _ y: Rational) -> Rational {
        return makeRat(numer(x) * denom(y), denom(x) * numer(y))
    }
    func isEqualRat(x: Rational, _ y: Rational) -> Bool {
        return (numer(x) * denom(y)) == (numer(y) * denom(x))
    }
    
    // Interface to rest of the system
    func tag(x: Rational) -> Tagged<Rational> { return attachTag("rational-number", value: x) }
    
    put("add", TypeKey(types: ["rational-number", "rational-number"]), { x, y in return tag(addRat(x,y)) } )
    put("sub", TypeKey(types: ["rational-number", "rational-number"]), { x, y in return tag(subRat(x,y)) } )
    put("mul", TypeKey(types: ["rational-number", "rational-number"]), { x, y in return tag(mulRat(x,y)) } )
    put("div", TypeKey(types: ["rational-number", "rational-number"]), { x, y in return tag(divRat(x,y)) } )
    put("make", TypeKey(types: ["rational-number", "rational-number"]), { x, y in return tag(makeRat(x,y)) } )
}


func makeRationalNumber(n: Double, d: Double) -> Tagged<Rational> {
    if let make = get("make", TypeKey(types: ["rational-number", "rational-number"])) as? (Double, Double) -> Tagged<Rational> {
        return make(n, d)
    } else {
        fatalError("makeSchemeNumber hasn't been implemented")
    }
}

installRationalPackage()
// let c = makeRationalNumber(3, d: 4)

//: We can install a similar package to handle complex numbers, using the tag complex. In creating the package, we extract from the table the operations make-from-real-imag and make-from-mag-ang that were defined by the rectangular and polar packages. Additivity permits us to use, as the internal operations, the same add-complex, sub-complex, mul-complex, and div-complex procedures from Section 2.4.1.

func installComplexPackage() {
    // Imported procedures from rectangular and polar packages
    
}



func +<T> (lhs: Tagged<T>, rhs: Tagged<T>) -> Tagged<T> {
    if let add = get("add", TypeKey(types: [lhs.key, rhs.key])) as? (T, T) -> Tagged<T> {
        return add(lhs.value, rhs.value)
    } else {
        fatalError("Addition is not installed for types: \(TypeKey(types: [lhs.key, rhs.key])))")
    }
}

(a + b).value


//: ## 2.5.2 Combining Data of Different Types
//:
//: We have seen how to define a unified arithmetic system that encompasses ordinary numbers, complex numbers, rational numbers, and any other type of number we might decide to invent, but we have ignorde, an important issue. The operations we have defined so far treat the different data types as being competely independent. Thus, there are separate packages for adding, say, two ordinary numbers, or two complex numbers. What we have not yet considered is the fact that it is meaningful to define operations that cross the type boundaries, such as the addition of a complex number to an ordinary number. We have gone to great pains to introduce barriers between parts of our programs so that they can be developed and understood separately. We would like to introduce the cross-type operations in some carefully controlled way, so that we can support them without seriously violating our module boundaries.
//:
//: One way to handle cross-type operations is to design a different procedure for each possible combination of types for which the operation is valid. For example. We could extend the complex-number package so that it provides a procedure for adding complex numbers to ordinary numbers and installs this in the table using the tag(complex scheme-number):
