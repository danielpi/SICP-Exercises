// Playground - noun: a place where people can play

import Cocoa

// Exercise 1.31
// a) The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures. Write an analogous procedure called product that returns the product of the values of a function at points over a given range. 

protocol MultipliableType: ExpressibleByIntegerLiteral {
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



func cube<T:MultipliableType>(_ x: T) -> T {
    return x * x * x
}
func identity<T>(_ x: T) -> T {
    return x
}
func inc(_ n: Int) -> Int {
    return n + 1
}
func isEven(_ n: Int) -> Bool {
    return (n % 2) == 0
}


func product<T:Comparable,U:MultipliableType>(_ term:(T) -> U, _ a:T, _ next:(T) -> T, _ b:T) -> U {
    if a > b {
        return 1
    } else {
        return term(a) * product(term, next(a), next, b)
    }
}

// Show how to define factorial in terms of product.

func factorial(_ n: Int) -> Int {
    return product(identity, 1, inc, n)
}
factorial(5)

// Use product to compute approximations to pi using the formula
/*

pi   2 * 4 * 4 * 6 * 6 * 8 ...
-- = -------------------------
 4   3 * 3 * 5 * 5 * 7 * 7 ...

*/

func DRPpi(_ steps: Int) -> Double {
    func term(_ k: Int) -> Double {
        if isEven(k) {
            return (Double(k) + 2) / (Double(k) + 1)
        } else {
            return (Double(k) + 1) / (Double(k) + 2)
        }
    }
    return 4 * product(term, 1, inc, steps)
}

DRPpi(1000)




