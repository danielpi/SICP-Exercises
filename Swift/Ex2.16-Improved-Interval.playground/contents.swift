import Cocoa

// Exercise 2.16
// Explain, in general, why equivalent algebraic expressions may lead to different answers. Can you devise an interval-arithmetic package that does not have this shortcoming, or is this task impossible? (Warning: This problem is very difficult)

// Interval operations involving intervals with some width/uncertainty will tend to increase their uncertainty with each operation. Thus if two algabreically equivilent procedures are performed and one uses more interval operations that include uncertain intervals then the final answers will be different.

struct EngineeringTolerance {
    var value: Double
    var tolerance: Double
    var lower: Double {
        get {
            return value - width
        }
    }
    var upper: Double {
        get {
            return value + width
        }
    }
    var width: Double {
        get {
            return value * tolerance
        }
    }
    
    init(_ value: Double, _ tolerance: Double) {
        self.value = value
        self.tolerance = tolerance
    }
}

func +(lhs: EngineeringTolerance, rhs: EngineeringTolerance) -> EngineeringTolerance {
    let newValue = lhs.value + rhs.value
    let newTolerance = (lhs.width + rhs.width) / newValue
    return EngineeringTolerance(newValue, newTolerance)
}

func *(lhs: EngineeringTolerance, rhs: EngineeringTolerance) -> EngineeringTolerance {
    return EngineeringTolerance(lhs.value * rhs.value, lhs.tolerance + rhs.tolerance)
}

func /(lhs: EngineeringTolerance, rhs: EngineeringTolerance) -> EngineeringTolerance {
    return EngineeringTolerance(lhs.value / rhs.value, lhs.tolerance - rhs.tolerance)
}

func parOne(a: EngineeringTolerance, _ b: EngineeringTolerance) -> EngineeringTolerance {
    return (a * b) / (a + b)
}

func parTwo(a: EngineeringTolerance, _ b: EngineeringTolerance) -> EngineeringTolerance {
    let one = EngineeringTolerance(1, 0)
    return one / ((one / a) + (one / b))
}

let r1 = EngineeringTolerance(3.3, 0.01)
let r2 = EngineeringTolerance(4.7, 0.05)

let a = parOne(r1, r2)
let b = parTwo(r1, r2)
// 0.02652330018327665

func series(r1: EngineeringTolerance, _ r2: EngineeringTolerance) -> EngineeringTolerance {
    return r1 + r2
}

let r3 = series(r1, r2)
r3.upper
r3.lower
r1.upper + r2.upper
r1.lower + r2.lower

((r1.upper + r2.upper) - 8) / 8
8 - (r1.lower + r2.lower)


// Looks like this would work, needs to be tested better though.





