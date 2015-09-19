//: Playground - noun: a place where people can play

import Cocoa

// Exercise 2.10
// Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and comments that it is not clear what it means to divide by an interval that spans zero. Modify Alyssa's code to check for this condition and to signal an error if it occurs.

struct Interval {
    var lower: Double
    var upper: Double
    var width: Double {
        get {
            return (upper - lower) / 2
        }
    }
    
    init(_ l: Double, _ u: Double) {
        self.lower = min(l,u)
        self.upper = max(l,u)
    }
}

func +(lhs: Interval, rhs: Interval) -> Interval {
    return Interval(lhs.lower + rhs.lower, lhs.upper + rhs.upper)
}

func -(lhs: Interval, rhs: Interval) -> Interval {
    return Interval(lhs.lower - rhs.lower, lhs.upper - rhs.upper)
}

func *(lhs: Interval, rhs: Interval) -> Interval {
    let p1 = lhs.lower * rhs.lower
    let p2 = lhs.lower * rhs.upper
    let p3 = lhs.upper * rhs.lower
    let p4 = lhs.upper * rhs.upper
    return Interval(min(p1, p2, p3, p4), max(p1, p2, p3, p4))
}

func isPositive(x: Double) -> Bool {
    return x > 0
}
func isNegative(x: Double) -> Bool {
    return x < 0
}

func / (lhs: Interval, rhs: Interval) -> Interval {
    if (isNegative(lhs.lower) && isPositive(lhs.upper) || isNegative(rhs.lower) && isPositive(rhs.upper)) {
        assertionFailure("Can't handle Intervals that span zero")
    }
    return lhs * Interval(1 / rhs.upper, 1 / rhs.lower)
}

let a = Interval(0.2, 0.3)
let b = Interval(1, 2)

b / a

