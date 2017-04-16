import Cocoa

// Exercise 2.12
// Define a constructor make-center-percent that takes a center and a percentage tolerance and produces the desired interval. You must also define a selector percent that produces the percentage tolerance for a given interval. The center selector is the same as the one shown above.

struct Interval {
    var lower: Double
    var upper: Double
    var center: Double {
        get {
            return (lower + upper) / 2
        }
    }
    var width: Double {
        get {
            return (upper - lower) / 2
        }
    }
    var percent: Double {
        get {
            return width / center
        }
    }
    
    init(_ l: Double, _ u: Double) {
        self.lower = min(l,u)
        self.upper = max(l,u)
    }
    init(center: Double, width: Double) {
        self.lower = center - width
        self.upper = center + width
    }
    init(center: Double, percent: Double) {
        self.init(center: center, width: center * percent)
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

let a = Interval(center: 3.3, percent: 0.05)
a.center
a.width
a.percent
// Two resistor sin series
let b = a + a
b.center
b.width
b.percent



