import Cocoa

// Exercise 2.13
// Show that under the assumption of small percentage tolerances there is a simple formula for the approcimate percentage tolerance of the product of two intervals in terms of the tolerances of the factors. You may simplify the problem by assuming that all numbers are positive.

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


let a = Interval(center: 47, percent: 0.01)
let b = Interval(center: 82, percent: 0.01)


let c = a * b
c.center
c.percent

// Looks like the approximate percentage tolerance of two intervals that are multiplied together is the additon of the two percentage tolerances of the two factors.


let d = Interval(center: 1.0, percent: 0.4)
let e = d * d
e.center
e.percent

// This starts to break down when the percent gets large, such as around 40% of the center.

