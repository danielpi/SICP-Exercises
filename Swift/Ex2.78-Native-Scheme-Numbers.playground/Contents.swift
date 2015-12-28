import Foundation

//: ## Exercise 2.78
//: The internal procedures in the scheme-number package are essentially nothing more than calls to the primitive procedures +, -, etc. It was not possible to use the primitives of the language directly because our type-tag system requires that each data object have a type attached to it. In fact, however, all Lisp implementations do have a type system, which they use internally. Primitive predicates such as symbol? and number? determine whether data objects have particular types. 
//:
//: Modify the definitions of type-tag, contents, and attach-tag from Section 2.4.2 so that our generic system takes advantage of Scheme's internal type system. That is to say, the system should work as before except that ordinary numbers should be represented simply as Scheme numbers rather than as pairs whose car is the symbol scheme-number.

// I don't see how I can do this with the system I have built in Swift. 

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

func attachTag<V>(tag: NumberType, value: V) -> Tagged<V> {
    return Tagged(type: tag, value: value)
}

struct TypeKey: Equatable, Hashable {
    let types: [NumberType]
    
    // FIXME: this is a terrible hashing algorithm
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

func applyGeneric(op: String, _ args: Tagged<Pair> ...) -> Tagged<Pair> {
    let typeTags = TypeKey(types: args.map { $0.type })
    switch typeTags.types.count {
    case 1:
        let proc = get(op, typeTags) as! (Tagged<Pair>) -> Tagged<Pair>
        return proc(args[0])
    case 2:
        let proc = get(op, typeTags) as! (Tagged<Pair>, Tagged<Pair>) -> Tagged<Pair>
        return proc(args[0], args[1])
    default:
        fatalError("There is no selector named \(op) for data of type \(typeTags) registered with \(globalSelectorTable)")
    }
}

func add(x: Tagged<Pair>, y: Tagged<Pair>) -> Tagged<Pair> {
    return applyGeneric("add", x, y)
}
func sub(x: Tagged<Pair>, y: Tagged<Pair>) -> Tagged<Pair> {
    return applyGeneric("sub", x, y)
}
func mul(x: Tagged<Pair>, y: Tagged<Pair>) -> Tagged<Pair> {
    return applyGeneric("mul", x, y)
}
func div(x: Tagged<Pair>, y: Tagged<Pair>) -> Tagged<Pair> {
    return applyGeneric("div", x, y)
}

func realPart(z: Tagged<Pair>) -> Double {
    // return applyGeneric("realPart", z) // Can't use applyGeneric beacuse these functions should return proper Swift types.
    if let proc = get("realPart", TypeKey(types: [z.type])) as? (Pair) -> Double {
        return proc(z.value)
    } else {
        fatalError("realPart Failed")
    }
}
func imagPart(z: Tagged<Pair>) -> Double {
    if let proc = get("imagPart", TypeKey(types: [z.type])) as? (Pair) -> Double {
        return proc(z.value)
    } else {
        fatalError("imagPart Failed")
    }
}
func magnitude(z: Tagged<Pair>) -> Double {
    if let proc = get("magnitude", TypeKey(types: [z.type])) as? (Pair) -> Double {
        return proc(z.value)
    } else {
        fatalError("magnitude Failed")
    }
}
func angle(z: Tagged<Pair>) -> Double {
    if let proc = get("angle", TypeKey(types: [z.type])) as? (Pair) -> Double {
        return proc(z.value)
    } else {
        fatalError("angle Failed")
    }
}

func installSwiftNumberPackage() {
    func tag(x: Double) -> Tagged<Double> { return attachTag(.number, value: x) }
    put("add", TypeKey(types: [.number, .number]), { x, y in tag(x + y) })
    put("sub", TypeKey(types: [.number, .number]), { x, y in tag(x - y) })
    put("mul", TypeKey(types: [.number, .number]), { x, y in tag(x * y) })
    put("div", TypeKey(types: [.number, .number]), { x, y in tag(x / y) })
    put("make", TypeKey(types: [.number, .number]), { x in tag(x) })
}

func makeSchemeNumber(n: Double) -> Tagged<Double> {
    if let make = get("make", TypeKey(types: [.number, .number])) as? (Double) -> Tagged<Double> {
        return make(n)
    } else {
        fatalError("makeSchemeNumber hasn't been implemented")
    }
}

struct Pair: CustomStringConvertible {
    let car: Any
    let cdr: Any
    
    var description: String {
        return "(\(car), \(cdr))"
    }
}

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
    put("makeFromRealImag", TypeKey(types: [.complex]), makeFromRealImag)
    put("makeFromMagAng", TypeKey(types: [.complex]), makeFromMagAng)
    
    // Added by Alyssa P. Hacker
    put("realPart", TypeKey(types: [.complex]), realPart)
    put("imagPart", TypeKey(types: [.complex]), imagPart)
    put("magnitude", TypeKey(types: [.complex]), magnitude)
    put("angle", TypeKey(types: [.complex]), angle)
}
installComplexPackage()

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


let z = makeComplexFromRealImag(3, y: 4)
globalSelectorTable

magnitude(z) // Gives an error

