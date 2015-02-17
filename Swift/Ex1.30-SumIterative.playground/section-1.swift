// Playground - noun: a place where people can play

import Cocoa

// Exercise 1.30
// The sum procedure generates a linear recursion. The procedure can be rewritten so that the sum is performed iteratively. Show how to do this by filling in the missing expressions

/*
(define (sum term a next b)
   (define (iter a result)
     (if <??>
         <??>
         (iter <??> <??>)))
   (iter <??> <??>))
*/


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


func sumIter<T:Comparable,U:AddableType>(term:(T) -> U, a:T, next:(T) -> T, b:T, result:U) -> U {
    if a > b {
        return result
    } else {
        return sumIter(term, next(a), next, b, term(a) + result)
    }
}

func sum<T:Comparable,U:AddableType>(term:(T) -> U, a:T, next:(T) -> T, b:T) -> U {
    return sumIter(term, a, next, b, 0)
}


func integral(f:(Double) -> Double, a:Double, b:Double, n:Int) -> Double {
    func h(a: Double, b: Double, n: Int) -> Double {
        return (b - a) / Double(n)
    }
    func yk(f:(Double) -> Double, a: Double, b: Double, n:Int, k:Int) -> Double {
        let x = a + (Double(k) * h(a,b,n))
        return f(x)
    }
    func simpsonsTerm(k:Int) -> Double {
        switch true {
        case k == 0 || k == n:
            return yk(f, a, b, n, k)
        case isEven(k):
            return 2 * yk(f, a, b, n, k)
        default:
            return 4 * yk(f, a, b, n, k)
        }
    }
    return (h(a,b,n) / 3.0) * sum(simpsonsTerm, 0, inc, n)
}
integral(cube, 0, 1, 100)
//integral(cube, 0, 1, 1000)
integral(identity, 0, 1, 100)
//integral(identity, 0, 1, 1000)
