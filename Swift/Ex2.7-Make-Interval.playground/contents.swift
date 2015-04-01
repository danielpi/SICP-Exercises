//: Playground - noun: a place where people can play

import Cocoa

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

