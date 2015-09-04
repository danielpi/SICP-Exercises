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


func operator_(exp: Expr) -> String {
    switch exp {
    case .Sum(_, _):
        return "+"
    case .Product(_, _):
        return "*"
    default:
        fatalError("Unhandled expression: \(exp)")
    }
}
func operands(exp: Expr) -> (Expr,Expr) {
    switch exp {
    case let .Sum(a, b):
        return (a.unbox, b.unbox)
    case let .Product(a, b):
        return (a.unbox, b.unbox)
    default:
        fatalError("Unhandled expression: \(exp)")
    }
}

//println(deriv2("x" + 3, "x")) // 1
//println(deriv2("x" * "y", "x")) // y
//println(deriv2(("x" * "y") * ("x" + 3), "x")) //

/*
case .Sum(_, _):
return makeSum(deriv(addend(exp), variable), deriv(augend(exp), variable))
case .Product(_, _):
return makeSum(makeProduct(multiplier(exp), deriv(multiplicand(exp), variable)), makeProduct(deriv(multiplier(exp), variable), multiplicand(exp)))
*/

//: - Explain what was done above. Why can't we assimilate the predicates number? and variable? into the data-directed dispatch?



//: - Write the procedures for derivatives of sums and products, and the auxiliary code required to install them in the table used by the program above.

func deriv2(exp: Expr, variable: Expr) -> Expr {
    switch exp {
    case .Constant(_):
        return .Constant(0)
    case .Variable(_):
        return isSameVariable(exp, variable) ? .Constant(1) : .Constant(0)
    default:
        let oper = operator_(exp)
        let rands = operands(exp)
        let function = get("deriv", operator_(exp))!
        print(variable)
        let result = function(exp: exp, variable: variable)
        return result
    }
}


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

//: -Choose any additional differentiation rule that you like, such as the one for exponents (Exercise 2.56), and install it in this data-directed system.
//: - In this simple algebraic manipulator the type of an expression is the algebraic operator that binds it together. Suppose, however, we indexed the procedures in the opposite way, so that the dispatch line in deriv looked like ((get (operator exp) 'deriv) (operands exp) var) What corresponding changes to the derivative system are required?
