import Cocoa

// Exercise 1.36
// Modify fixedPoint so that it prints the sequence of approximations it generates. Then find a solution to x^x = 1000 by finding a fixed point of x -> log(1000)/log(x). Compare the number of steps this takes with and without average damping.


func isCloseEnough(a: Double, _ b: Double, _ tolerance: Double) -> Bool {
    return abs(a - b) < tolerance
}

func fixedPoint(f: (Double) -> Double, _ guess: Double) -> Double {
    let next = f(guess)
    if isCloseEnough(guess, next, 0.00001) {
        return next
    } else {
        return fixedPoint(f, next)
    }
}

func solvXPowX(result: Double) -> Double {
    return fixedPoint({ (x: Double) -> Double in return log(result) / log(x) }, 2.0)
}
solvXPowX(1000.0)

func average(a: Double, _ b: Double) -> Double {
    return (a + b) / 2
}

func dampedSolvXPowX(result: Double) -> Double {
    return fixedPoint({ (x: Double) -> Double in return average(x, log(result)/log(x)) }, 2.0)
}
dampedSolvXPowX(1000.0)


