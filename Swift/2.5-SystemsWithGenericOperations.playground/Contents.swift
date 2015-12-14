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
struct Tagged<V> {
    let key: String
    let value: V
}

func attachTag<V>(tag: String, value: V) -> Tagged<V> {
    return Tagged(key: tag, value: value)
}

typealias Function = (Double, Double) -> Tagged<Double>
typealias Constructor = (Double) -> Tagged<Double>

struct TypeKey: Equatable, Hashable {
    let lhs: String
    let rhs: String
    
    var hashValue: Int {
        return (lhs + rhs).hashValue
    }
}
func == (lhs: TypeKey, rhs: TypeKey) -> Bool {
    return lhs.lhs == rhs.lhs && rhs.rhs == lhs.rhs
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

var globalConstructorTable = [String: [String: Constructor]]()
func put(op: String,_ type: String,_ item: Constructor) {
    if let _ = globalConstructorTable[type] {
        globalConstructorTable[type]![op] = item
    } else {
        globalConstructorTable[type] = [op: item]
    }
}
func get(op: String, _ type: String) -> Constructor? {
    return globalConstructorTable[type]?[op]
}

func installSwiftNumberPackage() {
    func tag(x: Double) -> Tagged<Double> { return attachTag("swift-number", value: x) }
    put("add", TypeKey(lhs: "swift-number", rhs: "swift-number"), { x, y in tag(x + y) })
    put("sub", TypeKey(lhs: "swift-number", rhs: "swift-number"), { x, y in tag(x - y) })
    put("mul", TypeKey(lhs: "swift-number", rhs: "swift-number"), { x, y in tag(x * y) })
    put("div", TypeKey(lhs: "swift-number", rhs: "swift-number"), { x, y in tag(x / y) })
    put("make", "swift-number", { x in tag(x) })
}

//: Users of the Scheme-number package will create (tagged) ordinary numbers by means of the procedure:

func makeSchemeNumber(n: Double) -> Tagged<Double> {
    return get("make", "swift-number")!(n)
}

installSwiftNumberPackage()
let a = makeSchemeNumber(9.4)
let b = makeSchemeNumber(12.8)

func +<T> (lhs: Tagged<T>, rhs: Tagged<T>) -> Tagged<T> {
    if let add = get("add", TypeKey(lhs: lhs.key, rhs: rhs.key)) as? (T, T) -> Tagged<T> {
        return add(lhs.value, rhs.value)
    } else {
        fatalError("Addition is not installed for types: \(TypeKey(lhs: lhs.key, rhs: rhs.key)))")
    }
}

(a + b).value


//: Now that the framework of the generic arithmetic system is in place, we can readily include new kinds of numbers. Here is a package that performs rational arithmetic. Notice that, as a benefit of additivity, we can use without modification the rational-number code from Section 2.1.1 as the internal procedures in the package:

func installRationalPackage() {
    // Internal Procedures from Ex 2.1.1
    enum ConsPosition {
        case Left, Right
    }
    
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
    
    typealias Rational = (ConsPosition -> Int)
    
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
    
    //put("add", TypeKey(lhs: "rational-number", rhs: "rational-number"), { x, y in return tag(addRat(x,y)) } )
}

// TODO: Figure out how best to get the put function to work in Swift

//: We can install a similar package to handle complex numbers, using the tag complex. In creating the package, we extract from the table the operations make-from-real-imag and make-from-mag-ang that were defined by the rectangular and polar packages. Additivity permits us to use, as the internal operations, the same add-complex, sub-complex, mul-complex, and div-complex procedures from Section 2.4.1.

func installComplexPackage() {
    // Imported procedures from rectangular and polar packages
    
}





