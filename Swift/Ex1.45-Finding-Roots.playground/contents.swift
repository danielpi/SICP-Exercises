import Cocoa

// Exercise 1.45
// We saw in section 1.3.3 that attempting to compute square roots by naively finding a fixed point of y -> x/y does not converg, and that this can be fixed by average damping. The same method works for finding cube roots as fixed points of the average-damped y -> x/y^2. Unfortunately, the process does not work for fourth roots -- a single average damp is not enough to make a fixed-point search for y -> x/y^3 converge. On the other hand, if we average damp twice the fixed-point search does converge.

// Do some experiments to determine how many average damps are required to compute nth roots as a fixed point search based upon repeated average damping of y -> x/y^n-1. 

// Use this to implement a simple procedure for computing nth roots using fixed-point, averageDamp and the repeated procedure of exercise 1.43.

func average(a: Double, b: Double) -> Double {
    return (a + b) / 2
}
func isCloseEnough(a: Double, b: Double, tolerance: Double) -> Bool {
    return abs(a - b) < tolerance
}

func fixedPoint(f: (Double) -> Double, guess: Double) -> Double {
    let next = f(guess)
    if isCloseEnough(guess, next, 0.00001) {
        return next
    } else {
        return fixedPoint(f, next)
    }
}

func averageDamp(f: (Double) -> Double) -> (Double) -> Double {
    return { (x: Double) -> Double in return average(x, f(x)) }
}


func nthRoot(x: Double, n: Int) -> Double {
    return fixedPoint(averageDamp({ (y: Double) -> Double in return x / pow(y, Double(n - 1)) }), 1.0)
}
nthRoot(2.0, 3)

pow(3.0, 3 - 1)

