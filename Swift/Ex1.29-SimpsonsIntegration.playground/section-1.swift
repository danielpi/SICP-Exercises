import Cocoa

// Exercise 1.29
// Simpson's Rule is a more accurate method of numerical integration than the method illustrated above in section 1.3. Using Simpson's Rule, the integral of a function f between a and b is approximated as ... [See pdf]

// Define a procedure that takes as arguments f, a, b and n and returns the value of the integral. Use your procedure to integrate cube between 0 and 1 (with n = 100 and n = 1000) and compare the results to those of the integral procedure shown above.


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



func cube<T:MultipliableType>(x: T) -> T {
    return x * x * x
}
func identity<T>(x: T) -> T {
    return x
}
func inc(n: Int) -> Int {
    return n + 1
}
func isEven(n: Int) -> Bool {
    return (n % 2) == 0
}


func sum<T:Comparable,U:AddableType>(term:(T) -> U, a:T, next:(T) -> T, b:T) -> U {
    if a > b {
        return 0
    } else {
        return term(a) + sum(term, a: next(a), next: next, b: b)
    }
}


func integral(f:(Double) -> Double, a:Double, b:Double, n:Int) -> Double {
    func h(a: Double, b: Double, n: Int) -> Double {
        return (b - a) / Double(n)
    }
    func yk(f:(Double) -> Double, a: Double, b: Double, n:Int, k:Int) -> Double {
        let x = a + (Double(k) * h(a,b: b,n: n))
        return f(x)
    }
    func simpsonsTerm(k:Int) -> Double {
        switch true {
        case k == 0 || k == n:
            return yk(f, a: a, b: b, n: n, k: k)
        case isEven(k):
            return 2 * yk(f, a: a, b: b, n: n, k: k)
        default:
            return 4 * yk(f, a: a, b: b, n: n, k: k)
        }
    }
    return (h(a,b: b,n: n) / 3.0) * sum(simpsonsTerm, a: 0, next: inc, b: n)
}
integral(cube, a: 0, b: 1, n: 100)
//integral(cube, 0, 1, 1000)
integral(identity, a: 0, b: 1, n: 100)
//integral(identity, 0, 1, 1000)
