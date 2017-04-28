import Cocoa

//: ## Exercise 2.58
//: Suppose we want to modify the differentiation program so that it works with ordinary mathematical notation, in which + and * are infix rather than prefix operators. Since the differentiation program is defined in terms of abstract data, we can modify it to work with different representations of expressions solely by changing the predicates, selectors and constructors that define the representation of the algebraic expressions on which the differentiator is to operate.

// This might be better if I were to go back the other way such that 
// deriv([* [* "x" "y"] [* "y" [+ "x" 3]]], "x")
// deriv([* "x" "y" [+ "x" 3]], "x")
// deriv("x" * "y" * ("x" + 3), "x")

indirect enum Expr {
    case Sum(Expr, Expr)
    case Product(Expr, Expr)
    case Exponential(Expr, Expr)
    case Constant(Int)
    case Variable(String)
}

extension Expr: ExpressibleByIntegerLiteral {
    init(integerLiteral value: IntegerLiteralType) {
        self = .Constant(value)
    }
}

extension Expr: ExpressibleByStringLiteral {
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

extension Expr: CustomStringConvertible {
    var description: String {
        switch self {
        case .Sum(let a1, let a2):
            return "(" + a1.description + " + " + a2.description + ")"
        case .Product(let m1, let m2):
            return "(" + m1.description + " * " + m2.description + ")"
        case .Exponential(let e1, let e2):
            return "(" + e1.description + " ** " + e2.description + ")"
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

precedencegroup ExponentPrecedence {
    higherThan: MultiplicationPrecedence
    associativity: left
}

// infix operator ** { associativity left precedence 160 }
infix operator **: ExponentPrecedence
func ** (lhs: Expr, rhs: Expr) -> Expr {
    return makeExponentiation(base: lhs, exponent: rhs)
}


func isVariable(_ exp: Expr) -> Bool {
    switch exp {
    case .Variable(_):
        return true
    default:
        return false
    }
}

func isSameVariable(_ v1: Expr, _ v2: Expr) -> Bool {
    switch (v1, v2) {
    case (.Variable(let val1), .Variable(let val2)):
        return val1 == val2
    default:
        return false
    }
}

func makeSum(_ a1: Expr, _ a2: Expr) -> Expr {
    switch (a1, a2) {
    case (.Constant(0), _):
        return a2
    case (_, .Constant(0)):
        return a1
    case (.Constant(let a), .Constant(let b)):
        return .Constant(a + b)
    default:
        return Expr.Sum(a1, a2)
    }
}

func isSum(_ exp: Expr) -> Bool {
    switch exp {
    case .Sum(_, _):
        return true
    default:
        return false
    }
}

func addend(_ s: Expr) -> Expr {
    switch s {
    case .Sum(let a1, _):
        return a1
    default:
        fatalError("Tried to get the addend from an expression that was not a sum")
    }
}

func augend(_ s: Expr) -> Expr {
    switch s {
    case .Sum(_, let a2):
        return a2
    default:
        fatalError("Tried to get the augend from an expression that was not a sum")
    }
}

func makeProduct(_ m1: Expr, _ m2: Expr) -> Expr {
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
        return Expr.Product(m1, m2)
    }
}

func isProduct(_ x: Expr) -> Bool {
    switch x {
    case .Product(_, _):
        return true
    default:
        return false
    }
}

func multiplier(_ p: Expr) -> Expr {
    switch p {
    case .Product(let m1, _):
        return m1
    default:
        fatalError("Tried to get the multiplier from an expression that was not a product")
    }
}

func multiplicand(_ p: Expr) -> Expr {
    switch p {
    case .Product(_, let m2):
        return m2
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
        return Expr.Exponential(base, exponent)
    }
}

func isExponentiation(_ exp: Expr) -> Bool {
    switch exp {
    case .Exponential(_, _):
        return true
    default:
        return false
    }
}

func base(_ exp: Expr) -> Expr {
    switch exp {
    case .Exponential(let b, _):
        return b
    default:
        fatalError("Tried to get the base from an expression that was not an Exponential")
    }
}

func exponent(_ exp: Expr) -> Expr {
    switch exp {
    case .Exponential(_, let e):
        return e
    default:
        fatalError("Tried to get the exponent from an expression that was not an Exponential")
    }
}


func deriv(_ exp: Expr, variable: Expr) -> Expr {
    switch exp {
    case .Constant(_):
        return .Constant(0)
    case .Variable(_):
        return isSameVariable(exp, variable) ? .Constant(1) : .Constant(0)
    case .Sum(_, _):
        return makeSum(deriv(addend(exp), variable:variable), deriv(augend(exp), variable:variable))
    case .Product(_, _):
        return makeSum(makeProduct(multiplier(exp), deriv(multiplicand(exp), variable:variable)), makeProduct(deriv(multiplier(exp), variable:variable), multiplicand(exp)))
    case .Exponential(let base, let exponent):
        return makeProduct(makeProduct(exponent,
                                       makeExponentiation(base: base,
                                                          exponent: makeSum(exponent, Expr.Constant(-1)))), deriv(base, variable:variable))
    }
}


print(deriv("x" + 3, variable:"x")) // 1
print(deriv("x" * "y", variable:"x")) // y
print(deriv(("x" * "y") * ("x" + 3), variable:"x")) //
print(deriv(2 * ("x" ** 4) + (6 * "y" ** 2), variable:"y"))
print(deriv("x" + (3 * ("x" + ("y" + 2))), variable:"x"))
print(deriv("x" + 3 * ("x" + "y" + 2), variable:"x"))

