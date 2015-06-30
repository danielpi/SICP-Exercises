import Cocoa

//: # Exercise 2.56
//: Show how to extend the basic differentiator to handle more kinds of expressions. For instance, implement the differentiation rule
//:
//:    d(u^n)         du
//:    ------ = nu^n-1--
//:       dx          dx
//:
//: by adding a new clause to the deriv program and defining appropriate procedures exponentiation?, base, exponent, and make-exponentiation. (You may use the symbol ** to denote exponentiation.) Build in the rules that anything raised to the power 0 is 1 and anything raised to the power 1 is the thing itself.

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
    return makeSum(lhs, rhs)
}

func * (lhs: Expr, rhs: Expr) -> Expr {
    return makeProduct(lhs, rhs)
}

infix operator ** { associativity left precedence 160 }
func ** (lhs: Expr, rhs: Expr) -> Expr {
    return makeExponentiation(lhs, rhs)
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

func isExponentiation(exp: Expr) -> Bool {
    switch exp {
    case .Exponential(_, _):
        return true
    default:
        return false
    }
}

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
    case .Exponential(let b, let e):
        let base = b.unbox
        let exponent = e.unbox
        
        return makeProduct(makeProduct(exponent,
            makeExponentiation(base,
                makeSum(exponent, Expr.Constant(-1)))), deriv(base, variable))
    default:
        fatalError("unknown expression type: DERIV")
    }
}

println(deriv("x" + 3, "x")) // 1
println(deriv("x" * "y", "x")) // y
println(deriv(("x" * "y") * ("x" + 3), "x")) //
println(deriv(2 * ("x" ** 4) + (6 * "y" ** 2), "y"))




