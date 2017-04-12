import Cocoa

// Exercise 1.45
// We saw in section 1.3.3 that attempting to compute square roots by naively finding a fixed point of y -> x/y does not converg, and that this can be fixed by average damping. The same method works for finding cube roots as fixed points of the average-damped y -> x/y^2. Unfortunately, the process does not work for fourth roots -- a single average damp is not enough to make a fixed-point search for y -> x/y^3 converge. On the other hand, if we average damp twice the fixed-point search does converge.

// Do some experiments to determine how many average damps are required to compute nth roots as a fixed point search based upon repeated average damping of y -> x/y^n-1. 

// Use this to implement a simple procedure for computing nth roots using fixed-point, averageDamp and the repeated procedure of exercise 1.43.

func average(_ a: Double, _ b: Double) -> Double {
    return (a + b) / 2
}
func isCloseEnough(_ a: Double, _ b: Double, _ tolerance: Double) -> Bool {
    return abs(a - b) < tolerance
}

func fixedPoint(_ f:@escaping (Double) -> Double, _ guess: Double) -> Double {
    let next = f(guess)
    if isCloseEnough(guess, next, 0.00001) {
        return next
    } else {
        return fixedPoint(f, next)
    }
}

func averageDamp(_ f:@escaping (Double) -> Double) -> (Double) -> Double {
    return { (x: Double) -> Double in return average(x, f(x)) }
}

func compose<T>(_ f:@escaping (T) -> T, _ g:@escaping (T) -> T) -> (T) -> T {
    return { (x: T) -> T in return f(g(x)) }
}

func repeatIter<T>(_ f:@escaping (T) -> T, _ g:@escaping (T) -> T, _ step: Int) -> (T) -> T {
    if (step == 1) {
        return g
    } else {
        return repeatIter(f, compose(f, g), step - 1)
    }
}

func repeated<T>(_ f:@escaping (T) -> T , _ n: Int) -> (T) -> T {
    return repeatIter(f, f, n)
}


func nthRoot(_ x: Double, _ n: Int) -> Double {
    let dampings = floor(log(Double(n)) / log(2))
    let damper = repeated(averageDamp, Int(dampings))
    return fixedPoint(damper({ (y: Double) -> Double in return x / pow(y, Double(n - 1)) }), 1.0)
}

                // Dampings
//nthRoot(2.0, 1) // 1
nthRoot(2.0, 2) // 1
nthRoot(2.0, 3) // 1
nthRoot(2.0, 4) // 2
nthRoot(2.0, 5) // 2
nthRoot(2.0, 6) // 2
nthRoot(2.0, 7) // 2
nthRoot(2.0, 8) // 3
nthRoot(2.0, 9) // 3

