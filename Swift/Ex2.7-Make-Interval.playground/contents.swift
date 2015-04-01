//: Playground - noun: a place where people can play

import Cocoa

// Exercise 2.7
// Alyssa's program is incomplete because she has not specified the implementation of the interval abstraction. Define an interval constructor as well as the upper-bound and lower-bound access functions.

struct Interval {
    var lower: Double
    var upper: Double
    init(_ l: Double, _ u: Double) {
        self.lower = min(l,u)
        self.upper = max(l,u)
    }
}

let a = Interval(6.7, 6.9)
let b = Interval(2.1, 2.3)

a.lower
a.upper
b.lower
b.upper

