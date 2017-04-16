import Cocoa

// Exercise 2.14
// Demonstrate that Lem is right. Investigate the behavior of the system on a variety of arithmetic expressions. Make some intervals A and B, and use them in computing the expressions A/A and A/B. You will get the most insight by using intervals whose width is a small percentage of the center value. Examine the results of the computation in center-percent form.


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


func parOne(_ a: Interval, _ b: Interval) -> Interval {
    return (a * b) / (a + b)
}

func parTwo(_ a: Interval, _ b: Interval) -> Interval {
    let one = Interval(1, 1)
    return one / ((one / a) + (one / b))
}

let r1 = Interval(center: 3.3, percent: 0.01)
let r2 = Interval(center: 4.7, percent: 0.05)

let pOne = parOne(r1, r2)
pOne.center
pOne.percent
let pTwo = parTwo(r1, r2)
pTwo.center
pTwo.percent

// As we can see Lem is right, parOne and parTwo give different results.

let aDiva = r1 / r1
aDiva.center
aDiva.percent
let aDivb = r1 / r2
aDivb.center
aDivb.percent

let one = Interval(1, 1)
let c = r1 / one
c.center
c.percent
let d = one / r1
d.center
d.percent
let e = d * r1
e.center
e.percent


var A = Interval(center: 1.0, percent: 0.01)
func mulDivide(_ a: Interval) -> Interval {
    return a * a / a
}
A = mulDivide(A)
A = mulDivide(A)    // As we perform more algebraic operations
A = mulDivide(A)    // our spread increases
A = mulDivide(A)    // Which is in line with the thought that our condifence in the answer
                    // is diminised.






