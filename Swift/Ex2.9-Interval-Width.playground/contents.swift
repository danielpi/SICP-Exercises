//: Playground - noun: a place where people can play

import Cocoa

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

func /(lhs: Interval, rhs: Interval) -> Interval {
    return lhs * Interval(1 / rhs.upper, 1 / rhs.lower)
}


// Exercise 2.9
// The width of an interval is half of the difference between its upper and lower bounds. The width measure of the uncertainty of the number specified by the interval. For some arithmetic operations the width of the result of combining two intervals is a function only of the widths of the argument intervals, whereas for others the width of the combination is not a function of the widths of the argument intervals. Show that the width of the sum (or difference) of two intervals is a function only of the widths of the intervals being added (or subtracted). Give examples to show that this is not true for multiplication or division.


let a = Interval(3.8, 4.0)
let b = Interval(2.0, 2.2)

let c = a + b
c.width
let cWidth = a.width + b.width

let d = a * b
d.width
let notDWidth = a.width * b.width




