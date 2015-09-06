import Cocoa

//: **Exercise 2.73:** Section 2.3.2 described a program that performs symbolic differentiation:

class Box<T>{
    let unbox: T
    init(_ value: T) {
        self.unbox = value
    }
}

enum Expr {
    case Sum(Box<Expr>, Box<Expr>)
    case Product(Box<Expr>, Box<Expr>)
    case Exponential(Box<Expr>, Box<Expr>)
    case Constant(Int)
    case Variable(String)
}

extension Expr: IntegerLiteralConvertible {
    init(integerLiteral value: IntegerLiteralType) {
        self = .Constant(value)
    }
}

extension Expr: StringLiteralConvertible {
    init(stringLiteral value: String) {
        self = .Variable(value)
    }
    
    init(extendedGraphemeClusterLiteral value: String) {
        self = .Variable(value)
    }
    
    init(unicodeScalarLiteral value: String) {
        self = .Variable(value)
    }
}

extension Expr: Printable {
    var description: String {
        switch self {
        case .Sum(let a1, let a2):
            return "(" + a1.unbox.description + " + " + a2.unbox.description + ")"
        case .Product(let m1, let m2):
            return "(" + m1.unbox.description + " * " + m2.unbox.description + ")"
        case .Constant(let value):
            return String(value)
        case .Variable(let label):
            return label
        case .Exponential(let e1, let e2):
            return "(" + e1.unbox.description + " ** " + e2.unbox.description + ")"
        }
    }
}

extension Expr: Equatable { }
func == (lhs: Expr, rhs: Expr) -> Bool {
    switch (lhs, rhs) {
    case let (.Variable(a),.Variable(b)):
        return a == b
    case let (.Constant(a), .Constant(b)):
        return a == b
    case let (.Sum(a,c), .Sum(b,d)):
        return (a.unbox == b.unbox) && (c.unbox == d.unbox)
    case let(.Product(a,c), .Product(b,d)):
        return (a.unbox == b.unbox) && (c.unbox == d.unbox)
    default:
        return false
    }
}


extension Expr: Hashable {
    var hashValue: Int {
        switch self {
        case let .Sum(a, b):
            return a.unbox.hashValue + b.unbox.hashValue
        case let .Product(a, b):
            return a.unbox.hashValue * b.unbox.hashValue
        case let .Constant(a):
            return a.hashValue
        case let .Variable(a):
            return a.hashValue
        case let .Exponential(base, exp):
            return Int(pow(Double(base.unbox.hashValue), Double(exp.unbox.hashValue)))
        }
    }
}


func + (lhs: Expr, rhs: Expr) -> Expr {
    return makeSum(lhs, rhs)
}

func * (lhs: Expr, rhs: Expr) -> Expr {
    return makeProduct(lhs, rhs)
}

func isVariable<T>(exp: Expr) -> Bool {
    switch exp {
    case .Variable(_):
        return true
    default:
        return false
    }
}

func isSameVariable(v1: Expr, v2: Expr) -> Bool {
    switch (v1, v2) {
    case (.Variable(let val1), .Variable(let val2)):
        return val1 == val2
    default:
        return false
    }
}

func makeSum1(a1:Expr, a2: Expr) -> Expr {
    return Expr.Sum(Box(a1), Box(a2))
}
func makeProduct1(m1: Expr, m2: Expr) -> Expr {
    return Expr.Product(Box(m1), Box(m2))
}

func isSum(exp: Expr) -> Bool {
    switch exp {
    case .Sum(_, _):
        return true
    default:
        return false
    }
}

func addend(s: Expr) -> Expr {
    switch s {
    case .Sum(let a1, let a2):
        return a1.unbox
    default:
        fatalError("Tried to get the addend from an expression that was not a sum")
    }
}

func augend(s: Expr) -> Expr {
    switch s {
    case .Sum(let a1, let a2):
        return a2.unbox
    default:
        fatalError("Tried to get the augend from an expression that was not a sum")
    }
}

func isProduct(x: Expr) -> Bool {
    switch x {
    case .Product(_, _):
        return true
    default:
        return false
    }
}
func multiplier(p: Expr) -> Expr {
    switch p {
    case .Product(let m1, _):
        return m1.unbox
    default:
        fatalError("Tried to get the multiplier from an expression that was not a product")
    }
}
func multiplicand(p: Expr) -> Expr {
    switch p {
    case .Product(_, let m2):
        return m2.unbox
    default:
        fatalError("Tried to get the multiplicand from an expression that was not a product")
    }
}
func makeSum(a1: Expr, a2: Expr) -> Expr {
    switch (a1, a2) {
    case (.Constant(0), _):
        return a2
    case (_, .Constant(0)):
        return a1
    case (.Constant(let a), .Constant(let b)):
        return .Constant(a + b)
    default:
        return Expr.Sum(Box(a1), Box(a2))
    }
}
func makeProduct(m1: Expr, m2: Expr) -> Expr {
    switch (m1, m2) {
    case (.Constant(0), _):
        return .Constant(0)
    case (_, .Constant(0)):
        return .Constant(0)
    case (.Constant(1), _):
        return m2
    case (_, .Constant(1)):
        return m1
    case (.Constant(let a), .Constant(let b)):
        return .Constant(a * b)
    default:
        return Expr.Product(Box(m1), Box(m2))
    }
}

func deriv(exp: Expr, variable: Expr) -> Expr {
    switch exp {
    case .Constant(_):
        return .Constant(0)
    case .Variable(_):
        return isSameVariable(exp, variable) ? .Constant(1) : .Constant(0)
    case .Sum(_, _):
        return makeSum(deriv(addend(exp), variable), deriv(augend(exp), variable))
    case .Product(_, _):
        return makeSum(makeProduct(multiplier(exp), deriv(multiplicand(exp), variable)), makeProduct(deriv(multiplier(exp), variable), multiplicand(exp)))
    default:
        fatalError("unknown expression type: DERIV")
    }
}

println(deriv("x" + 3, "x"))                    // 1
println(deriv("x" * "y", "x"))                  // y
println(deriv(("x" * "y") * ("x" + 3), "x"))    // ((x * y) + (y * (x + 3)))


//: We can regard this program as performing a dispatch on the type of the expression to be differentiated. In this situation the "type tag" of the datum is the algebraic operator symbol (such as +) and the operation being performed is deriv. We can transform this program into data-directed style by rewriting the basic derivative procedure as


typealias DerivativeFunction = (exp: Expr, variable: Expr) -> Expr

var globalSelectorTable = [String: [String: DerivativeFunction]]()

func put(op: String, type: String, item: DerivativeFunction) {
    if let typeColumn = globalSelectorTable[type] {
        globalSelectorTable[type]![op] = item
    } else {
        globalSelectorTable[type] = [op: item]
    }
}


func get(op: String, type: String) -> DerivativeFunction? {
    return globalSelectorTable[type]?[op]
}


func operatorAsString(exp: Expr) -> String {
    switch exp {
    case .Sum(_, _):
        return "+"
    case .Product(_, _):
        return "*"
    case .Exponential(_, _):
        return "**"
    default:
        fatalError("Unhandled expression: \(exp)")
    }
}

func deriv2(exp: Expr, variable: Expr) -> Expr {
    switch exp {
    case .Constant(_):
        return .Constant(0)
    case .Variable(_):
        return isSameVariable(exp, variable) ? .Constant(1) : .Constant(0)
    default:
        let function = get("deriv", operatorAsString(exp))!
        return function(exp: exp, variable: variable)
    }
}

//: - Explain what was done above. Why can't we assimilate the predicates number? and variable? into the data-directed dispatch?

// For the operator types we have switched out static dispatching for a lookup mechanism that allows us to install new operator types. In the swift case we could assimilate the .Constant and .Variable types if we wanted to. This is because we have effectively tagged them by defining them as a part of the Expr enum. In the original Scheme code though the variables are symbols and the constants are just numbers which means their type is difficult to look up in a table???


//: - Write the procedures for derivatives of sums and products, and the auxiliary code required to install them in the table used by the program above.

func installDerivativeSumPackage() {
    // Internal Procedures
    func makeSum(a1: Expr, a2: Expr) -> Expr {
        switch (a1, a2) {
        case (.Constant(0), _):
            return a2
        case (_, .Constant(0)):
            return a1
        case (.Constant(let a), .Constant(let b)):
            return .Constant(a + b)
        default:
            return Expr.Sum(Box(a1), Box(a2))
        }
    }
    
    func derivSum(exp: Expr, variable: Expr) -> Expr {
        return makeSum(deriv2(addend(exp), variable), deriv2(augend(exp), variable))
    }
    
    // Interface to the rest of the system
    put("deriv", "+", derivSum)
}

installDerivativeSumPackage()
globalSelectorTable

func installDerivativeProductPackage() {
    func makeProduct(m1: Expr, m2: Expr) -> Expr {
        switch (m1, m2) {
        case (.Constant(0), _):
            return .Constant(0)
        case (_, .Constant(0)):
            return .Constant(0)
        case (.Constant(1), _):
            return m2
        case (_, .Constant(1)):
            return m1
        case (.Constant(let a), .Constant(let b)):
            return .Constant(a * b)
        default:
            return Expr.Product(Box(m1), Box(m2))
        }
    }
    
    func derivProduct(exp: Expr, variable: Expr) -> Expr {
        return makeSum(makeProduct(multiplier(exp), deriv2(multiplicand(exp), variable)), makeProduct(deriv2(multiplier(exp), variable), multiplicand(exp)))
    }
    
    put("deriv", "*", derivProduct)
}

installDerivativeProductPackage()
globalSelectorTable


println(deriv2("x", "x"))                       // 1
println(deriv2(4, "x"))                         // 0
println(deriv2("x" + 3, "x"))                   // 1
println(deriv2("x" * "y", "x"))                 // y
println(deriv2(("x" * "y") * ("x" + 3), "x"))   // ((x * y) + (y * (x + 3)))

//: - Choose any additional differentiation rule that you like, such as the one for exponents (Exercise 2.56), and install it in this data-directed system.

func installDerivativeExponentPackage() {
    func base(exp: Expr) -> Expr {
        switch exp {
        case .Exponential(let b, _):
            return b.unbox
        default:
            fatalError("Tried to get the base from an expression that was not an Exponential")
        }
    }
    
    func exponent(exp: Expr) -> Expr {
        switch exp {
        case .Exponential(_, let e):
            return e.unbox
        default:
            fatalError("Tried to get the exponent from an expression that was not an Exponential")
        }
    }
    
    func makeSum(a1: Expr, a2: Expr) -> Expr {
        switch (a1, a2) {
        case (.Constant(0), _):
            return a2
        case (_, .Constant(0)):
            return a1
        case (.Constant(let a), .Constant(let b)):
            return .Constant(a + b)
        default:
            return Expr.Sum(Box(a1), Box(a2))
        }
    }
    
    func makeProduct(m1: Expr, m2: Expr) -> Expr {
        switch (m1, m2) {
        case (.Constant(0), _):
            return .Constant(0)
        case (_, .Constant(0)):
            return .Constant(0)
        case (.Constant(1), _):
            return m2
        case (_, .Constant(1)):
            return m1
        case (.Constant(let a), .Constant(let b)):
            return .Constant(a * b)
        default:
            return Expr.Product(Box(m1), Box(m2))
        }
    }
    
    func makeExponentiation(base: Expr, exponent:Expr) -> Expr {
        switch (base, exponent) {
        case (_, .Constant(0)):
            return .Constant(1)
        case (_, .Constant(1)):
            return base
        case (.Constant(let b), .Constant(let e)):
            return Expr.Constant(Int(pow(Double(b),Double(e))))
        default:
            return Expr.Exponential(Box(base), Box(exponent))
        }
    }
    
    func derivExponentiation(exp: Expr, variable: Expr) -> Expr{
        let base = base(exp)
        let exponent = exponent(exp)
        
        return makeProduct(makeProduct(exponent,
                makeExponentiation(base,
                 makeSum(exponent, Expr.Constant(-1)))), deriv2(base, variable))
    }
    
    put("make", "**", makeExponentiation)
    put("deriv", "**", derivExponentiation)
}

installDerivativeExponentPackage()

infix operator ** { associativity left precedence 160 }
func ** (lhs: Expr, rhs: Expr) -> Expr {
    let makeExponentiation = get("make", "**")
    return makeExponentiation!(exp: lhs, variable: rhs)
}

println(globalSelectorTable)

println(deriv2("x" ** 4, "x"))

println(deriv2("x", "x"))                       // 1
println(deriv2(4, "x"))                         // 0
println(deriv2("x" + 3, "x"))                   // 1
println(deriv2("x" * "y", "x"))                 // y
println(deriv2(("x" * "y") * ("x" + 3), "x"))   // ((x * y) + (y * (x + 3)))
println(deriv2(2 * ("x" ** 4) + (6 * "y" ** 2), "y"))

//: - In this simple algebraic manipulator the type of an expression is the algebraic operator that binds it together. Suppose, however, we indexed the procedures in the opposite way, so that the dispatch line in deriv looked like ((get (operator exp) 'deriv) (operands exp) var) What corresponding changes to the derivative system are required?

// Shouldn't look much different at all, just need to swap the rows and columns in the function table.
