//: Playground - noun: a place where people can play

import Cocoa

// Exercise 2.8
// Using reasoning analogous to Alyssa's, describe how the difference of two intervals may be computed. Define a corresponding subtraction procedure, called sub-interval.

struct Interval {
    var lower: Double
    var upper: Double
    init(_ l: Double, _ u: Double) {
        self.lower = min(l,u)
        self.upper = max(l,u)
    }
}

// The interval created when you subtract the lower bound of a from the lower bound of b and the upper bound of a from the upper bound of b should give a reasonable subtraction function.

let a = Interval(1.9, 2.1)
let b = Interval(0.9, 1.1)

func -(lhs: Interval, rhs: Interval) -> Interval {
    return Interval(lhs.lower - rhs.lower, lhs.upper - rhs.upper)
}

a - b

