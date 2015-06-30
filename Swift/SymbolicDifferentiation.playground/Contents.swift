import Cocoa

class Box<T>{
    let unbox: T
    init(_ value: T) {
        self.unbox = value
    }
}

// How do I represent a function
// Expr.Function(type: String, payload: Expr)
// Expr.Function("sin", payload: "x")
//
// Could print it with 
// return type + "(" + payload.description + ")"
//
// But not sure how to handle input. 
// String literal: "sin" "x"

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
        case .Exponential(let e1, let e2):
            return "(" + e1.unbox.description + " ** " + e2.unbox.description + ")"
        case .Constant(let value):
            return String(value)
        case .Variable(let label):
            return label
        }
    }
}

func + (lhs: Expr, rhs: Expr) -> Expr {
    switch (lhs, rhs) {
    case (.Constant(0), _):
        return rhs
    case (_, .Constant(0)):
        return lhs
    case (.Constant(let a), .Constant(let b)):
        return .Constant(a + b)
    default:
        return Expr.Sum(Box(lhs), Box(rhs))
    }
}

func - (lhs: Expr, rhs: Expr) -> Expr {
    return lhs + (-1 * rhs)
}

func * (lhs: Expr, rhs: Expr) -> Expr {
    switch (lhs, rhs) {
    case (.Constant(0), _):
        return .Constant(0)
    case (_, .Constant(0)):
        return .Constant(0)
    case (.Constant(1), _):
        return rhs
    case (_, .Constant(1)):
        return lhs
    case (.Constant(let a), .Constant(let b)):
        return .Constant(a * b)
    default:
        return Expr.Product(Box(lhs), Box(rhs))
    }
}

infix operator ** { associativity left precedence 160 }
func ** (base: Expr, exponent: Expr) -> Expr {
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

func == (lhs: Expr, rhs: Expr) -> Bool {
    switch (lhs, rhs) {
    case (.Constant(let a), .Constant(let b)):
        return a == b
    case (.Variable(let a), .Variable(let b)):
        return a == b
    case (.Sum(let a, let b), .Sum(let c, let d)):
        return a.unbox == c.unbox && b.unbox == d.unbox
    case (.Product(let a, let b), .Product(let c, let d)):
        return a.unbox == c.unbox && b.unbox == d.unbox
    case (.Exponential(let a, let b), .Product(let c, let d)):
        return a.unbox == c.unbox && b.unbox == d.unbox
    default:
        return false
    }
}


func deriv(exp: Expr, variable: Expr) -> Expr {
    switch exp {
    case .Constant(_):
        return .Constant(0)
    case .Variable(_):
        return exp == variable ? .Constant(1) : .Constant(0)
    case .Sum(let lhs, let rhs):
        let addend = lhs.unbox
        let augend = rhs.unbox
        return deriv(addend, variable) + deriv(augend, variable)
    case .Product(let lhs, let rhs):
        let multiplier = lhs.unbox
        let multiplicand = rhs.unbox
        return multiplier * deriv(multiplicand, variable) + deriv(multiplier, variable) * multiplicand
    case .Exponential(let lhs, let rhs):
        let base = lhs.unbox
        let exponent = rhs.unbox
        return exponent * base ** (exponent - 1) * deriv(base, variable)
    default:
        fatalError("unknown expression type: DERIV")
    }
}

println(deriv("x" + 3, "x")) // 1
println(deriv("x" * "y", "x")) // y
println(deriv(("x" * "y") * ("x" + 3), "x")) //
println(deriv(2 * ("x" ** 4) + (6 * "y" ** 2), "y"))
println(deriv("x" + (3 * ("x" + ("y" + 2))), "x"))
println(deriv("x" + 3 * ("x" + "y" + 2), "x"))

