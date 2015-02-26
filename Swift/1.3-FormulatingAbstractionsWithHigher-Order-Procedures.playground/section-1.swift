import Cocoa

protocol MultipliableType {
    func *(lhs: Self, rhs: Self) -> Self
}
extension Double : MultipliableType {}
extension Float  : MultipliableType {}
extension Int    : MultipliableType {}

protocol AddableType: IntegerLiteralConvertible {
    func +(lhs: Self, rhs: Self) -> Self
}
extension Double : AddableType {}
extension Float  : AddableType {}
extension Int    : AddableType {}



// 1.3 Formulating Abstractions with Higher-Order Procedures

// Procedures are abstractions that describe compound operations on numbers independent of the particular numbers. For instance.

func cube<T:MultipliableType>(x: T) -> T {
    return x * x * x
}

// We are not talking about the cube of a particular number, but rather a method of obtaining the cube of any number. This is not strictly nessesary however a language without this ability would be at a serious disadvantage. You would be limited to the set of primitive operations that were a part of the language.
// Yet even in numerical processing we will be severely limited in our ability to create abstractions if we are restricted to procedures whose parameters must be numbers. Higher-order procedures are procedures that can accept or return other procedures.


// 1.3.1 Procedures as Arguments
// Consider the following three procedures

func sumIntegers(a: Int, b: Int) -> Int {
    if a > b {
        return 0
    } else {
        return a + sumIntegers(a + 1, b)
    }
}
sumIntegers(1, 10)

func sumCubes(a: Int, b: Int) -> Int {
    if a > b {
        return 0
    } else {
        return cube(a) + sumCubes(a + 1, b)
    }
}
sumCubes(1, 10)

func piSum(a: Int, b: Int) -> Float {
    if a > b {
        return 0
    } else {
        return (1.0 / (Float(a) * (Float(a) + 2.0))) + piSum(a + 4, b)
    }
}
piSum(1, 100) * 8

// These three procedures clearly share a common underlying pattern. We could generate each of the procedures by filling in slots in the same template
/*
func <name>(a: Int, b: Int) -> ??? {
    if a > b {
        return 0
    } else {
        return <term>(a) + <name>(<next>(a), b)
    }
}
*/

// The pattern described above is very similar to the mathematical concept of summation. This allows mathematicians to deal with the concept of summation rather than simply a particular instance of summation.

func sum<T:Comparable,U:AddableType>(term:(T) -> U, a:T, next:(T) -> T, b:T) -> U {
    if a > b {
        return 0
    } else {
        return term(a) + sum(term, next(a), next, b)
    }
}

// We can recreate the procedures from above using our sum() function.

func inc(n: Int) -> Int {
    return n + 1
}
func cubeDouble(x: Int) -> Double {
    return Double(x * x * x)
}
func sumCubes2(a: Int, b: Int) -> Double {
    return sum(cubeDouble, a, inc, b)
}
sumCubes2(1, 10)

func identity<T>(x:T) -> T {
    return x
}
func sumIntegers2(a: Int, b: Int) -> Int {
    return sum(identity, a, inc, b)
}
sumIntegers2(1, 10)

func piSum2(a:Int, b:Int) -> Double {
    func piTerm(x: Int) -> Double {
        return 1.0 / (Double(x) * (Double(x) + 2.0))
    }
    func piNext(x: Int) -> Int {
        return x + 4
    }
    return sum(piTerm, a, piNext, b)
}
8 * piSum2(1, 1000)


// Once we have sum() we can use it as a building block if formulating further concepts. For instance the definite integral of a function f between limits a and b can be approximated numerically. We can express this directly as a procedure using the following

func integral(f:(Double) -> Double, a:Double, b:Double, dx:Double) -> Double {
    func addDx(x:Double) -> Double {
        return x + dx
    }
    return sum(f, a + (2 * dx), addDx, b) * dx
}
integral(cube, 0, 1, 0.01)
//integral(cube, 0, 1, 0.001)
//integral(identity, 0, 1, 0.01)
//integral(identity, 0, 1, 0.001)



// 1.3.2 Constructing Procedures Using Lambda (Closures)
// In piSum it is a bit awkward having to define piTerm and piNext just so that they can be used as arguments to our higher-order procedure. We can do this with closures

func piSum3(a:Int, b:Int) -> Double {
    return sum({ (x: Int) -> Double in
        return 1.0 / (Double(x) * (Double(x) + 2.0)) },
        a,
        { (x: Int) -> Int in
            return x + 4 },
        b)
}
8 * piSum3(1, 1000)

// Again using closures we can write integral without having to define auxilary procedure addDx

func integral2(f:(Double) -> Double, a:Double, b:Double, dx:Double) -> Double {
    return sum(f, a + (2 * dx), { (x:Double) -> Double in return x + dx }, b) * dx
}
integral2(cube, 0, 1, 0.01)

// Closures take the general form
// { (<args>) -> <return> in <body> }

// The resulting procedure is just as much a procedure as one that is created using func. The only difference is that it has not been associated with any name. In fact

func plus4(x: Int) -> Int {
    return x + 4
}
plus4(4)

// is equivilent to
var plus5 = { (x: Int) -> Int in return x + 5 }
plus5(5)


// Using let to create local variables
// I'm not sure if there is an equivilent in Swift for this sort of thing. I'll implement the examples as best I can

func square(x:Int) -> Int {
    return x * x
}
square(4)

func f(x:Int, y:Int) -> Int {
    func fHelper(a:Int, b:Int) -> Int {
        return (x * square(a)) + (y * b) + (a * b)
    }
    return fHelper((x * y) + 1, 1 - y)
}
f(3,4)

// Or we could use a closure to accomplish this

func f2(x: Int, y: Int) -> Int {
    return { (a:Int, b:Int) -> Int in
        return (x * square(a)) + (y * b) + (a * b)
    }((x * y) + 1, 1 - y)
}
f2(3,4)

// But that is ugly as sin.

// In Lisp there is a let construct that makes this all simpler. Is this the same as specifying local variables in Swift?

func f3(x: Int, y: Int) -> Int {
    let a = (x * y) + 1
    let b = 1 - y
    return (x * square(a)) + (y * b) + (a * b)
}
f3(3,4)

// The above isn't the same as a let construct from lisp. At this point I'm not sure why it is significant though.

// Let allows one to bind variables as locally as possible to where they are to be used. For example if the value of x is 5, the value of the expression

func letDemo(x: Int) -> Int {
    return { (x:Int) -> Int in return (x * 10) + x }(3) + x
}
letDemo(5)

// is 38. Here the x in the body of the let is 3, so the value of the let expression is 33. On the other hand, the x that is the second argument to the outermost addition is still 5.

// The variables values are computed outside the let. This matters when the expressions that provide the values for the local variables depend upon variables having the same names as the local variables themselves. For example, if the value of x is 2, the expression

func letDemo2(x: Int) -> Int {
    return { (z: Int, y: (Int) -> Int) -> Int in return z * y(x) }(3, { (x: Int) -> Int in return x + 2 } )
}
letDemo2(2)

// I could mimick the lisp let implementation here is Swift. Ignoring the fact that the above code is horrendous to look at, I also had to change the name of x to z so that swift would evaluate the intended scope for the first closure. I don't see why the Lisp version would even want to work the way it does/

// Sometimes we can use internal definitions to get the same effect as with let. For example

func f4(x: Int, y: Int) -> Int {
    func a() -> Int { return 1 + (x * y) }
    func b() -> Int { return 1 - y }
    return (x * square(a())) + (y * b()) + (a() * b())
}
f4(3,4)

// However let is preferred in situations like this and to use internal define only for internal procedures.

// How would a Swift version look

func f5(x: Int, y: Int) -> Int {
    let a = 1 + (x * y)
    let b = 1 - y
    return (x * square(a)) + (y * b) + (a * b)
}
f5(3,4)



