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
integral(cube, 0, 1, 0.001)
integral(identity, 0, 1, 0.01)
integral(identity, 0, 1, 0.001)
