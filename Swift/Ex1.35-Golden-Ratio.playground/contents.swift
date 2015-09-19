import Cocoa

// Exercise 1.35
// Show that the golden ration is a fixed point of the transformation x -> 1 + 1/x and use this fact to compute the golden ratio by means of the fixed-point procedure

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

let goldenRatio = fixedPoint({ (x:Double) -> Double in return 1 + (1/x)}, 1.0)
goldenRatio
let algebraicGoldenRatio = (1.0 + pow(5.0, 0.5)) / 2.0
goldenRatio - algebraicGoldenRatio



