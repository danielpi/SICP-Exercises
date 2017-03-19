
import Cocoa

//: ## 2.3.2 Example: Symbolic Differentiation
//: As an illustration of symbol manipulation and a further illustration of data abstraction, consider the design of a procedure that performs symbolic differentiation of algebraic expressions. We would like the procedure to take as arguments an algebraic expression and a variable and to return the derivative of the expression with respect to the variable. For example, if the arguments to the procedure are 
//:
//:     ax^2 + bx +c and x
//: 
//: the procedure should return 
//:
//:     2ax + bx + c
//: 
//: Symbolic differentiation is of special historical significance in Lisp. It was one of the motivation examples behind the development of a computer language for symbol manipulation. Furthermore, it marked the beginning of the line of research that led to the development of powerful systems for symbolic mathematical work, which are currently being used by a growing number of applied mathematicians and physicists.
//:
//: In developing the symbolic-differentiation program, we will follow the same strategy of data abstraction that we followed in developing the rational-number system of Section 2.1.1. That is, we will first define a differentiation algorithm that operates on abstract objects such as "sums," "products," and "variables" without worrying about how these are to be represented. Only afterward will we address the representation problem.
//:
//: ### The differentiation program with abstract data
//: In order to keep things simple, we will condsider a very simple symbolic-differentiation program that handles expressions that are built up using only the operations of addition and multiplication with two arguments. Differentiation of any such expression can be carried out by applying the following reduction rules:
//:
//:          dc
//:          -- = 0, for c, a constant or a variable different from x,
//:          dx
//:    
//:          dx
//:          -- = 1
//:          dx
//:
//:    d(u + v)   du   dv
//:    -------- = -- + --
//:       dx      dx   dx
//:
//:       d(uv)    dv    du
//:       ----- = u-- + v--
//:         dx     dx    dx
//:
//:
//: Observe that the latter two rules are recursive in nature. That is, to obtain the derivative of a sum we first find the derivatives of the terms and add them. Each of the terms may in turn be an expression that needs to be decomposed. Decomposing into smaller and smaller pieces will eventually produce pieces that are either constants or variables, whose derivatives will be either 0 or 1.
//:
//: To embody these rules in a procedure we indulge in a little wishful thinking, as we did in designing the rational-number implementation. If we had a means for representing algebraic expressions, we should be able to tell whether an expression is a sum, a product, a constant, or a variable. We should be able to extract the parts of an expression. For a sum, for example we want to be able to extract the addend (first term) and the augend (second term). We should also be able to construct expressions from parts. Let us assume that we already have procedures to implement the following selectors, constructors, and predicates:

//: TODO More stuff here but rushing through to code

//: ### Representing algebraic expressions
//: We can imagine many ways to use list structure to represent algebraic expressions. For example, we could use lists of symbols that mirror the usual algebraic notation, representing ax + b as the list (a * x + b). However, one especially straightforward choice is to use the same parenthesized prefix notation that Lisp uses for combinations; that is, to represent ax + b as (+ (* a x) b). Then our data representation for the differentiation problem is as follows:

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

extension Expr: CustomStringConvertible {
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


// The variables are symbols. The are identified by the primitive predicate symbol?:
func isVariable(_ exp: Expr) -> Bool {
    switch exp {
    case .Variable(_):
        return true
    default:
        return false
    }
}

// Two variables are the same if the symbols representing them are eq?
func isSameVariable(_ v1: Expr, _ v2: Expr) -> Bool {
    switch (v1, v2) {
    case (.Variable(let val1), .Variable(let val2)):
        return val1 == val2
    default:
        return false
    }
}

// Sums and products are constructed as lists:
func makeSum1(_ a1:Expr, _ a2: Expr) -> Expr {
    return Expr.Sum(Box(a1), Box(a2))
}
func makeProduct1(_ m1: Expr, _ m2: Expr) -> Expr {
    return Expr.Product(Box(m1), Box(m2))
}

// A sum is a list whose first element is the symbol + (Not here its not. A Sum is a case of our expr enum)
func isSum(_ exp: Expr) -> Bool {
    switch exp {
    case .Sum(_, _):
        return true
    default:
        return false
    }
}

// The addend is the second item of the sum list:
func addend(_ s: Expr) -> Expr {
    switch s {
    case .Sum(let a1, _):
        return a1.unbox
    default:
        fatalError("Tried to get the addend from an expression that was not a sum")
    }
}

// The augend is the third item of the sum list:
func augend(_ s: Expr) -> Expr {
    switch s {
    case .Sum(_, let a2):
        return a2.unbox
    default:
        fatalError("Tried to get the augend from an expression that was not a sum")
    }
}

// A product is a list whose first element is the symbol *:
func isProduct(_ x: Expr) -> Bool {
    switch x {
    case .Product(_, _):
        return true
    default:
        return false
    }
}

// The multiplier is the second item of the product list:
func multiplier(_ p: Expr) -> Expr {
    switch p {
    case .Product(let m1, _):
        return m1.unbox
    default:
        fatalError("Tried to get the multiplier from an expression that was not a product")
    }
}

// The multiplican is the third item of the product list:
func multiplicand(_ p: Expr) -> Expr {
    switch p {
    case .Product(_, let m2):
        return m2.unbox
    default:
        fatalError("Tried to get the multiplicand from an expression that was not a product")
    }
}



func deriv1(_ exp: Expr, _ variable: Expr) -> Expr {
    switch exp {
    case .Constant(_):
        return .Constant(0)
    case .Variable(_):
        return isSameVariable(exp, variable) ? .Constant(1) : .Constant(0)
    case .Sum(_, _):
        return makeSum1(deriv1(addend(exp), variable), deriv1(augend(exp), variable))
    case .Product(_, _):
        return makeSum1(makeProduct1(multiplier(exp), deriv1(multiplicand(exp), variable)), makeProduct1(deriv1(multiplier(exp), variable), multiplicand(exp)))
    default:
        fatalError("unknown expression type: DERIV")
    }
}


print(deriv1("x" + 3, "x")) // 1
print(deriv1("x" * "y", "x")) // y
print(deriv1(("x" * "y") * ("x" + 3), "x")) //


//: The program produces answers that are correct; however, they are unsimplified. It is true that
//:
//:     d(xy)
//:     ----- = x * 0 + 1 * y
//:       dx
//:
//: but we would like the program to know that x * 0 = 0, 1 * y = y, and 0 + y = y. The answer for the second example should have been simply y. As the third example shows, tis becomes a serious issue when the expressions are complex.
//:
//: Our difficulty is much like the one we encountered with the rational-number implementation: we haven't reduced answers to simplist form. To accomplish the rational-number reduction, we needed to change only the constructors and the selectors of the implementation. We can adopt a similar strategy here. We won't change deriv at all. Instead, we will change makeSum so that if both summands are numbers, makeSum will add them and return their sum. Also, if one of the summands is 0, then makeSum will return the other summand.

func makeSum(_ a1: Expr, _ a2: Expr) -> Expr {
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

//: This uses swifts pattern matching.

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
        return Expr.Product(Box(m1), Box(m2))
    }
}

//: Here is how this version works on our three examples:

func deriv(_ exp: Expr, _ variable: Expr) -> Expr {
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

print(deriv("x" + 3, "x")) // 1
print(deriv("x" * "y", "x")) // y
print(deriv(("x" * "y") * ("x" + 3), "x")) //

//: Although this is quite an improvement, the third example shows that there is still a long way to go before we get a program that puts expressions into a form that we might agree is "simplest". The problem of algebraic simplification is complex because, among other reasons, a form that may be simplest for one purpose may not be for another.






