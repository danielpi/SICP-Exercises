import Cocoa

protocol MultipliableType {
    static func * (lhs: Self, rhs: Self) -> Self
}
extension Double : MultipliableType {}
extension Float  : MultipliableType {}
extension Int    : MultipliableType {}

protocol AddableType: ExpressibleByIntegerLiteral {
    static func + (lhs: Self, rhs: Self) -> Self
}
extension Double : AddableType {}
extension Float  : AddableType {}
extension Int    : AddableType {}



// 1.3 Formulating Abstractions with Higher-Order Procedures

// Procedures are abstractions that describe compound operations on numbers independent of the particular numbers. For instance.

func cube<T:MultipliableType>(_ x: T) -> T {
    return x * x * x
}

// We are not talking about the cube of a particular number, but rather a method of obtaining the cube of any number. This is not strictly nessesary however a language without this ability would be at a serious disadvantage. You would be limited to the set of primitive operations that were a part of the language.
// Yet even in numerical processing we will be severely limited in our ability to create abstractions if we are restricted to procedures whose parameters must be numbers. Higher-order procedures are procedures that can accept or return other procedures.


// 1.3.1 Procedures as Arguments
// Consider the following three procedures

func sumIntegers(_ a: Int, _ b: Int) -> Int {
    if a > b {
        return 0
    } else {
        return a + sumIntegers(a + 1, b)
    }
}
sumIntegers(1, 10)

func sumCubes(_ a: Int, _ b: Int) -> Int {
    if a > b {
        return 0
    } else {
        return cube(a) + sumCubes(a + 1, b)
    }
}
sumCubes(1, 10)

func piSum(_ a: Int, _ b: Int) -> Float {
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
        return term(a) + sum(term: term, a: next(a), next: next, b: b)
    }
}

// We can recreate the procedures from above using our sum() function.

func inc(_ n: Int) -> Int {
    return n + 1
}
func cubeDouble(_ x: Int) -> Double {
    return Double(x * x * x)
}
func sumCubes2(_ a: Int, _ b: Int) -> Double {
    return sum(term: cubeDouble, a: a, next: inc, b: b)
}
sumCubes2(1, 10)

func identity<T>(_ x:T) -> T {
    return x
}
func sumIntegers2(_ a: Int, _ b: Int) -> Int {
    return sum(term: identity, a: a, next: inc, b: b)
}
sumIntegers2(1, 10)

func piSum2(_ a:Int, _ b:Int) -> Double {
    func piTerm(_ x: Int) -> Double {
        return 1.0 / (Double(x) * (Double(x) + 2.0))
    }
    func piNext(_ x: Int) -> Int {
        return x + 4
    }
    return sum(term: piTerm, a: a, next: piNext, b: b)
}
8 * piSum2(1, 1000)


// Once we have sum() we can use it as a building block if formulating further concepts. For instance the definite integral of a function f between limits a and b can be approximated numerically. We can express this directly as a procedure using the following

func integral(f:(Double) -> Double, a:Double, b:Double, dx:Double) -> Double {
    func addDx(x:Double) -> Double {
        return x + dx
    }
    return sum(term: f, a: a + (2 * dx), next: addDx, b: b) * dx
}
integral(f: cube, a: 0, b: 1, dx: 0.01)
//integral(cube, 0, 1, 0.001)
//integral(identity, 0, 1, 0.01)
//integral(identity, 0, 1, 0.001)



// 1.3.2 Constructing Procedures Using Lambda (Closures)
// In piSum it is a bit awkward having to define piTerm and piNext just so that they can be used as arguments to our higher-order procedure. We can do this with closures

func piSum3(_ a:Int, _ b:Int) -> Double {
    return sum(term: { (x: Int) -> Double in return 1.0 / (Double(x) * (Double(x) + 2.0)) },
                  a: a,
               next: { (x: Int) -> Int in return x + 4 },
                  b: b)
}
8 * piSum3(1, 1000)

// Again using closures we can write integral without having to define auxilary procedure addDx

func integral2(f:(Double) -> Double, a:Double, b:Double, dx:Double) -> Double {
    return sum(term: f, a: a + (2 * dx), next: { (x:Double) -> Double in return x + dx }, b: b) * dx
}
integral2(f: cube, a: 0, b: 1, dx: 0.01)

// Closures take the general form
// { (<args>) -> <return> in <body> }

// The resulting procedure is just as much a procedure as one that is created using func. The only difference is that it has not been associated with any name. In fact

func plus4(_ x: Int) -> Int {
    return x + 4
}
plus4(4)

// is equivilent to
var plus5 = { (x: Int) -> Int in return x + 5 }
plus5(5)


// Using let to create local variables
// I'm not sure if there is an equivilent in Swift for this sort of thing. I'll implement the examples as best I can

func square(_ x:Int) -> Int {
    return x * x
}
square(4)

func f(_ x:Int, _ y:Int) -> Int {
    func fHelper(_ a:Int, _ b:Int) -> Int {
        return (x * square(a)) + (y * b) + (a * b)
    }
    return fHelper((x * y) + 1, 1 - y)
}
f(3,4)

// Or we could use a closure to accomplish this

func f2(_ x: Int, _ y: Int) -> Int {
    return { (a:Int, b:Int) -> Int in
        return (x * square(a)) + (y * b) + (a * b)
    }((x * y) + 1, 1 - y)
}
f2(3,4)

// But that is ugly as sin.

// In Lisp there is a let construct that makes this all simpler. Is this the same as specifying local variables in Swift?

func f3(_ x: Int, _ y: Int) -> Int {
    let a = (x * y) + 1
    let b = 1 - y
    return (x * square(a)) + (y * b) + (a * b)
}
f3(3,4)

// The above isn't the same as a let construct from lisp. At this point I'm not sure why it is significant though.

// Let allows one to bind variables as locally as possible to where they are to be used. For example if the value of x is 5, the value of the expression

func letDemo(_ x: Int) -> Int {
    return { (x:Int) -> Int in return (x * 10) + x }(3) + x
}
letDemo(5)

// is 38. Here the x in the body of the let is 3, so the value of the let expression is 33. On the other hand, the x that is the second argument to the outermost addition is still 5.

// The variables values are computed outside the let. This matters when the expressions that provide the values for the local variables depend upon variables having the same names as the local variables themselves. For example, if the value of x is 2, the expression

func letDemo2(_ x: Int) -> Int {
    return { (z: Int, y: (Int) -> Int) -> Int in return z * y(x) }(3, { (x: Int) -> Int in return x + 2 } )
}
letDemo2(2)

// I could mimick the lisp let implementation here is Swift. Ignoring the fact that the above code is horrendous to look at, I also had to change the name of x to z so that swift would evaluate the intended scope for the first closure. I don't see why the Lisp version would even want to work the way it does/

// Sometimes we can use internal definitions to get the same effect as with let. For example

func f4(_ x: Int, _ y: Int) -> Int {
    func a() -> Int { return 1 + (x * y) }
    func b() -> Int { return 1 - y }
    return (x * square(a())) + (y * b()) + (a() * b())
}
f4(3,4)

// However let is preferred in situations like this and to use internal define only for internal procedures.

// How would a Swift version look

func f5(_ x: Int, _ y: Int) -> Int {
    let a = 1 + (x * y)
    let b = 1 - y
    return (x * square(a)) + (y * b) + (a * b)
}
f5(3,4)



// 1.3.3 Procedures as General Methods
// Finding roots of equations by the half-interval method

// The half-interval method is a simple but powerful technique for finding roots of an equation.

func average(_ a: Double, _ b: Double) -> Double {
    return (a + b) / 2
}
func isCloseEnough(_ a: Double, _ b: Double, tol tolerance: Double) -> Bool {
    return abs(a - b) < tolerance
}
func isPositive(_ x: Double) -> Bool {
    return x > 0
}
func isNegative(_ x: Double) -> Bool {
    return x < 0
}

func search(_ f:(Double) -> Double, _ negative: Double, _ positive: Double) -> Double {
    let midpoint = average(negative, positive)
    if isCloseEnough(negative, positive, tol: 0.001) {
        return midpoint
    } else {
        let testValue = f(midpoint)
        switch true {
        case isPositive(testValue):
            return search(f, negative, midpoint)
        case isNegative(testValue):
            return search(f, midpoint, positive)
        default:
            return midpoint
        }
    }
}

// Search is awkward to use directly because we can accidentally give it points at which f's values do not have the required sign. Instead we will use search via the following procedure.


func halfIntervalMethod(_ f:(Double) -> Double, _ a: Double, _ b: Double) -> Double? {
    let aValue = f(a)
    let bValue = f(b)
    switch true {
    case isNegative(aValue) && isPositive(bValue):
        return search(f, a, b)
    case isNegative(bValue) && isPositive(aValue):
        return search(f, b, a)
    default:
        return nil
    }
}

halfIntervalMethod(sin, 2.0, 4.0)

let root = halfIntervalMethod({ (x:Double) -> Double in (x * x * x) - (2 * x) - 3 }, 1.0, 2.0)
root

// Finding fixed points of functions
// Begin with an initial guess and applying f repeatedly until the value doesn't change very much.


func fixedPoint(_ f: (Double) -> Double, _ guess: Double) -> Double {
    let next = f(guess)
    if isCloseEnough(guess, next, tol: 0.00001) {
        return next
    } else {
        return fixedPoint(f, next)
    }
}
fixedPoint(cos, 1.0)
let aFixedPoint = fixedPoint({ (y:Double) -> Double in return sin(y) + cos(y) }, 1.0)
aFixedPoint

// The fixed point process is similar to square-root computation.

func sqrtInfiniteLoop(_ x: Double) -> Double {
    return fixedPoint({ (y:Double) -> Double in x / y }, 1.0)
}
//sqrtInfiniteLoop(16.0)

// The fixed point search above does not converge. The next guesses oscillate around the correct value. 

func sqrt(_ x: Double) -> Double {
    return fixedPoint({ (y: Double) -> Double in average(y, x/y) }, 1.0)
}
sqrt(2.0)



// 1.3.4 Procedures as Returned Values
// The above examples demonstrate how the ability to pass procedures as arguments significantly enhances the expressive power of our programming language. We can achieve even more expressive power by creating procedures whose returned values are themselves procedures.

// We can illustrate this idea by looking again at the fixed-point example described at the end of section 1.3.3. We formulated a new version of the square-root procedure as a fixed-point search, starting with the observation thatsqrt(x) is a fixed-point of the function y -> x/y. Then we used average dampingto make the approximations converge. Average damping is a useful general technique in itself. Namely, given a function f, we consider the function whose value at x is equal to the average of x and f(x).

func averageDamp(_ f: @escaping (Double) -> Double) -> (Double) -> Double {
    return { (x: Double) -> Double in return average(x, f(x)) }
}

func squareDouble(_ x: Double) -> Double {
    return x * x
}

averageDamp(squareDouble)(10)

// Using averageDamp we can reformulate the square-root procedure as follows

func sqrt2(_ x: Double) -> Double {
    return fixedPoint(averageDamp({ (y: Double) -> Double in return x / y }), 1.0)
}

sqrt2(64)

// Notice how this formulation makes explicit the three ideas in the method: fixed-point search, average damping and the function y -> x/y. It is instructive to compare this formulation of the square-root method with the original version given in section 1.1.7. Bear in mind that these procedures express the same process, and notice how much clearer the idea becomes when we express the process in terms of these abstractions. In general there are many ways to formulate a process as a procedure. Experienced programmers kmow how to choose procedureal formulations that are particularly perspicuous, and where useful elements of the process are exposed as separate entities that can be reused in other applications. As a simple example of reuse, notice that the cube root of x is a fixed point of the function y -> x/y^2, so we can immediately generalise our square-root procedure to one that extracts cube roots.

func cubeRoot(_ x: Double) -> Double {
    return fixedPoint(averageDamp({ (y: Double) -> Double in return x / (y * y) }), 1.0)
}

cubeRoot(27)


// Newton's Method
// When we first introduced the square root procedure in section 1.1.7 we mentioned that this was a special case of Newton's method. If x -> g(x) is a differentiable function, then a solution of the equation g(x) = 0 is a fixed point of the function x -> f(x) where
/*
              g(x)
  f(x) = x - -----
             Dg(x)
*/
// Dg(x) is the derivative of g evaluated at x. Newton's method is the use of the fixed-point method we saw above to approximate a solution of the equation by finding a fixed point of the function f. For many functions g and for sufficiently good initial guesses for x, Newton's method converges very rapidly to a solution of g(x) = 0

// In order to implement Newton's method as a procedure, we must first express the idea of derivative. Not that "derivative" like average damping, is something that transforms a function into another function. For instance the derivative of the function x -> x^3 is the function x -> 3x^2. In general if g is a function and dx is a small number then the derivative Dg of g is the function whose value at any number x is given by
/*
          g(x + dx) - g(x)
  Dg(x) = ----------------
                 dx
*/

// Thus we can express the idea of derivative as the procedure

func deriv(_ g: @escaping (Double) -> Double) -> (Double) -> Double {
    return { (x: Double) -> Double in
                let dx = 0.00001
                return (g(x + dx) - g(x)) / dx
            }
}

// Like averageDamp, deriv is a procedure that takes a procedure as argument and returns a procedure as value. For example to approximate the derivative of x -> x^3 at 5 (whose exact value is 75) we can evaluate

func cube(_ x: Double) -> Double { return x * x * x }
deriv(cube)(5)

let cubeDeriv = deriv(cube)

// Create an array of doubles. Have a start value and an end value and a linear growing value for each value in between

func linspace(start: Double, end: Double, steps: Int) -> [Double] {
    var array = [Double](repeating: 0.0, count: steps)
    let stepSize = (end - start) / Double(steps - 1)
    for (index, _) in array.enumerated() {
        array[index] = start + (stepSize * Double(index))
    }
    return array
}

let x = linspace(start: -10, end: 10, steps: 51)

for value in x {
    cube(value)
    cubeDeriv(value)
}

// With the aid of deriv we can express Newton's method as a fixed point process:

func newtonTransform(_ g: @escaping (Double) -> Double) -> (Double) -> Double {
    return { (x: Double) -> Double in return x - (g(x) / deriv(g)(x)) }
}
func newtonsMethod(_ g: @escaping (Double) -> Double, _ guess: Double) -> Double {
    return fixedPoint(newtonTransform(g), guess)
}

// To find the square root of x we can use Newton's method to find a zero of the function y -> y^2 - x starting with an initial guess of 1.

/*
sqrt(x) = ?
?^2 = x
?^2 - x = 0 when ? == sqrt(x)
*/

// This provides yet another form of the square root procedure

func sqrt3(_ x: Double) -> Double {
    return newtonsMethod({ (y: Double) -> Double in return (y * y) - x }, 1.0)
}
sqrt3(64)


// Abstractions and first-class procedures
// We've seen two ways to express the square-root computation as an instance of a more general method, 
//    1. As a fixed-pont search and
//    2. Using Newton's method
// Since Newton's method was itself expressed as a fixed-point process, we actually saw two ways to compute square roots as fixed points. Each method begins with a function and finds a fixed point of some transformation of the function. We can express this general idea itself as a procedure

func fixedPointOfTransform(_ g: @escaping (Double) -> Double,
                     transform: (@escaping (Double) -> Double) -> (Double) -> Double,
                         guess: Double) -> Double {
    return fixedPoint(transform(g), guess)
}

// This very general procedure takes as its arguments a procedure g that computes some function, a procedure that transforms g and an initial guess. The returned result is a fixed point of the transformed function.

// Using this abstraction we can recast the first square root computation as an instance of this general method.

func sqrt4(_ x: Double) -> Double {
    return fixedPointOfTransform({ (y:Double) -> Double in return x / y }, transform: averageDamp, guess: 1.0)
}
sqrt4(2)

// Similary we can express the second square-root computation from this section as

func sqrt5(_ x: Double) -> Double {
    return fixedPointOfTransform({ (y:Double) -> Double in return (y * y) - x }, transform: newtonTransform, guess: 1.0)
}
sqrt5(2)

// We began section 1.3 with the observation that compound procedures are a crucial abstraction mechanism, because they permit us to express general methods of computing as explicit elements in our programming language. Now we've seen how higher-order procedures permit us to manipulate these general methods to create further abstractions.

// As programmers we should be alert to opportunities to identify the underlying abstractions in our programs and to build upon them and generalise them to create more powerful abstractions. This is not to say that one should always write programs in the most abstract way possible, choose the right level of abstraction that is appropriate to the task. The significance of higher-order procedures is that they enable us to represent these abstractions expicitly as elements in our programming language, so that they can be handled just like other computational elements.

// In general, programming languages impose restricitons on the ways in which computational elements can be manipulated. Elements with the fewest restricitions are said to have first-class status. Some of the "rights and privileges" of first-class elements are 
//  - They may be named by variables
//  - They may be passed as arguments to procedures
//  - They may be returned as the results of procedures
//  - They may be included in data structures

// Swift, unlike other common programming languages, awards procedures full first-class status. This poses challenges for efficient implementation, but the resulting gain in expressive power is enormous.


