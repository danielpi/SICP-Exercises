//: Playground - noun: a place where people can play

import Cocoa

// Exercise 2.10
// In passing, Ben also cryptically comments: "By testing the signs of the endpoints of the intervals, it is possible to break * into nine cases, only one of which requires more than two multiplications." Rewrite this procedure using Ben's suggestion.

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

func /(lhs: Interval, rhs: Interval) -> Interval {
    if (isNegative(lhs.lower) && isPositive(lhs.upper) || isNegative(rhs.lower) && isPositive(rhs.upper)) {
        assertionFailure("Can't handle Intervals that span zero")
    }
    return lhs * Interval(1 / rhs.upper, 1 / rhs.lower)
}

func isPositive(x: Interval) -> Bool {
    return isPositive(x.lower) && isPositive(x.upper)
}
func spansZero(x: Interval) -> Bool {
    return isNegative(x.lower) && isPositive(x.upper)
}
func isNegative(x: Interval) -> Bool {
    return isNegative(x.lower) && isNegative(x.upper)
}

func mulInterval(lhs: Interval, rhs: Interval) -> Interval {
    switch true {
    case isPositive(lhs) && isPositive(rhs):
        return Interval(lhs.lower * rhs.lower, lhs.upper * rhs.upper)
    case spansZero(lhs) && isPositive(rhs):
        return Interval(lhs.lower * rhs.upper, lhs.upper * rhs.upper)
    case isNegative(lhs) && isPositive(rhs):
        return Interval(lhs.lower * rhs.upper, lhs.upper * rhs.upper)
    case isPositive(lhs) && spansZero(rhs):
        return Interval(lhs.upper * rhs.lower, lhs.upper * rhs.upper)
    case spansZero(lhs) && spansZero(rhs):
        return Interval(min(lhs.lower * rhs.lower, lhs.upper * rhs.lower), max(lhs.lower * rhs.lower, lhs.upper * rhs.upper))
    case isNegative(lhs) && spansZero(rhs):
        return Interval(lhs.lower * rhs.upper, lhs.lower * rhs.lower)
    case isPositive(lhs) && isNegative(rhs):
        return Interval(lhs.upper * rhs.lower, lhs.lower * rhs.upper)
    case spansZero(lhs) && isNegative(rhs):
        return Interval(lhs.upper * rhs.lower, lhs.lower * rhs.lower)
    case isNegative(lhs) && isNegative(rhs):
        return Interval(lhs.upper * rhs.upper, lhs.lower * rhs.lower)
    default:
        assertionFailure("mulInterval failed")
        return lhs
    }
}

let positive = Interval(1, 2)
let spans = Interval(-0.2, 0.3)
let negative = Interval(-3, -2)

mulInterval(positive, rhs: positive)
mulInterval(spans, rhs: positive)
mulInterval(negative, rhs: positive)
mulInterval(positive, rhs: spans)
mulInterval(spans, rhs: spans)
mulInterval(negative, rhs: spans)
mulInterval(positive, rhs: negative)
mulInterval(spans, rhs: negat