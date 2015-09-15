import Cocoa

// Exercise 2.15
// Eva Lu Ator, another user, has also noticed the different intervals computed by different but algebraically equivalent expressions. She says that a formula to compute with intervals using Alyssa's system will produce tighter error bounds if it can be written in such a form that no variable that represents an uncertain number is repeated. Thus, she says par2 is a "better" program for parallel resistances than par1. Is she right? Why?


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


func parOne(a: Interval, b: Interval) -> Interval {
    return (a * b) / (a + b)
}

func parTwo(a: Interval, b: Interval) -> Interval {
    let one = Interval(1, 1)
    return one / ((one / a) + (one / b))
}

let r1 = Interval(center: 3.3, percent: 0.01)
let r2 = Interval(center: 4.7, percent: 0.05)

let pOne = parOne(r1, b: r2)
pOne.center
pOne.percent
let pTwo = parTwo(r1, b: r2)
pTwo.center
pTwo.percent

// These intervls are representing a spread of probabilities regarding the possible results of sticking two actual resistors in parallel. Lets reduce this down to a small number of calculations to see what the spread should include

func par(r1: Double, r2: Double) -> Double {
    return (r1 * r2) / (r1 + r2)
}

// First the centers
let centers = par(r1.center, r2: r2.center)

// Now with the lower limits
let lowerLimits = par(r1.lower, r2: r2.lower)

// Now with the upper limits
let upperLimits  = par(r1.upper, r2: r2.upper)
let handMade = Interval(lowerLimits, upperLimits)
handMade.percent

// What about with mixed lower and upper values
let mixedOne = par(r1.lower, r2: r2.upper)
let mixedTwo = par(r1.upper, r2: r2.lower)

handMade - pOne
handMade - pTwo

// pTwo is most similar to handMade so I would agree that par2 is the b