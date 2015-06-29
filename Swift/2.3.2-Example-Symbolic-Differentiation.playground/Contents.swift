import Cocoa

//: ## 2.3.2 Example: Symbolic Differentiation
//: As an illustration of symbol manipulation and a further illustration of data abstraction, consider the design of a procedure that performs symbolic differentiation of algebraic expressions. We would like the procedure to takeas arguments an algebraic expression and a variable and to returnthe derivative of the expression with respect to the variable. For example, if the arguments to the procedure are ax^2 + bx +c and x, the procedure should return 2ax + bx + c. Symbolic differentiation is of special historical significance in Lisp. It was one of the motivation examples behind the development of a computer language for symbol manipulation. Furthermore, it marked the beginning of the line of research that led to the development of powerful systems for symbolic mathematical work, which are currently being used by a growing number of applied mathematicians and physicists.

//: In developing the symbolic-differentiation program, we will follow the same strategy of data abstraction that we followed in developing the rational-number system of Section 2.1.1. That is, we will first define a differentiation algorithm that operates on abstract objects such as "sums," "products," and "variables" without worrying about how these are to be represented. Only afterward will we address the representation problem.

//: ### The differentiation program with abstract data
//: In order to keep things simple, we will condsider a very simple symbolic-differentiation program that handles expressions that are built up using only the operations of addition and multiplication with two arguments. Differentiation of any such expression can be carried out by applying the following reduction rules:

//:          dc
//:          -- = 0, for c a constant or a variable different from x,
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

//: Observe that the latter two rules are recursive in nature. That is, to obtain the derivative of a sum we first find the derivatives of the terms and add them. Each of the terms may in turn be an expression that needs to be decomposed. Decomposing into smaller and smaller pieces will eventually produce pieces that are either constants or variables, whose derivatives will be either 0 or 1.

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

enum Expr: Printable {
    case Sum(Box<Expr>, Box<Expr>)
    case Product(Box<Expr>, Box<Expr>)
    case Constant(Int)
    case Variable(String)
    
    var description: String {
        switch self {
        case .Sum(let a1, let a2):
            return a1.unbox.description + " + " + a2.unbox.description
        case .Product(let m1, let m2):
            return m1.unbox.description + " * " + m2.unbox.description
        case .Constant(let value):
            return String(value)
        case .Variable(let label):
            return label
        }
    }
}

// The variables are symbols. The are identified by the primitive predicate symbol?:
func isVariable<T>(exp: Expr) -> Bool {
    switch exp {
    case .Variable(_):
        return true
    default:
        return false
    }
}

// Two variables are the same if the symbols representing them are eq?
func isSameVariable(v1: Expr, v2: Expr) -> Bool {
    switch (v1, v2) {
    case (.Variable(let val1), .Variable(let val2)):
        return val1 == val2
    default:
        return false
    }
}

// Sums and products are constructed as lists:
func makeSum(a1:Expr, a2: Expr) -> Expr {
    return Expr.Sum(Box(a1), Box(a2))
}
func makeProduct(m1: Expr, m2: Expr) -> Expr {
    return Expr.Product(Box(m1), Box(m2))
}

// A sum is a list whose first element is the symbol + (Not here its not. A Sum is a case of our expr enum)
func isSum(exp: Expr) -> Bool {
    switch exp {
    case .Sum(_, _):
        return true
    default:
        return false
    }
}

// The addend is the second item of the sum list:
func addend(s: Expr) -> Expr {
    switch s {
    case .Sum(let a1, let a2):
        return a1.unbox
    default:
        fatalError("Tried to get the addend from an expression that was not a sum")
    }
}

// The augend is the third item of the sum list:
func augend(s: Expr) -> Expr {
    switch s {
    case .Sum(let a1, let a2):
        return a2.unbox
    default:
        fatalError("Tried to get the augend from an expression that was not a sum")
    }
}

// A product is a list whose first element is the symbol *:
func isProduct(x: Expr) -> Bool {
    switch x {
    case .Product(_, _):
        return true
    default:
        return false
    }
}

// The multiplier is the second item of the product list:
func multiplier(p: Expr) -> Expr {
    switch p {
    case .Product(let m1, _):
        return m1.unbox
    default:
        fatalError("Tried to get the multiplier from an expression that was not a product")
    }
}

// The multiplican is the third item of the product list:
func multiplicand(p: Expr) -> Expr {
    switch p {
    case .Product(_, let m2):
        return m2.unbox
    default:
        fatalError("Tried to get the multiplicand from an expression that was not a product")
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

let expression1 = makeSum(Expr.Variable("x"), Expr.Constant(3))
println(expression1)
let deriv1 = deriv(makeSum(Expr.Variable("x"), Expr.Constant(3)), Expr.Variable("x"))
println(deriv1)



// deriv(exp: x + 3, variable: x) // 1
// deriv(exp: x * y, variable: x) // y
// deriv(exp: (x * y) * (x + 3), variable: x) //





