import Cocoa

// Exercise 1.40
// Define a procedure cubic that can be used together with the newtonsMethod procedure in expressions of the form newtonsMethod(cubic(a, b, c), 1.0) to approximate zeros of the cubic x^3 + ax^2 + bx + c

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

func deriv(g: (Double) -> Double) -> (Double) -> Double {
    return { (x: Double) -> Double in
        let dx = 0.00001
        return (g(x + dx) - g(x)) / dx
    }
}

func linspace(start: Double, end: Double, steps: Int) -> [Double] {
    var array = [Double](count: steps, repeatedValue: 0.0)
    let stepSize = (end - start) / Double(steps - 1)
    for (index, element) in enumerate(array) {
        array[index] = start + (stepSize * Double(index))
    }
    return array
}

func newtonTransform(g: (Double) -> Double) -> (Double) -> Double {
    return { (x: Double) -> Double in return x - (g(x) / deriv(g)(x)) }
}

func newtonsMethod(g: (Double) -> Double, guess: Double) -> Double {
    return fixedPoint(newtonTransform(g), guess)
}


func cubic(a: Double, b: Double, c: Double) -> (Double) -> Double {
    return { (x: Double) -> Double in return pow(x, 3) + (a * pow(x, 2)) + (b * x) + c }
}

newtonsMethod(cubic(5, 2.5, -9), -10.0)


