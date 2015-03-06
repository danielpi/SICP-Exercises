import Cocoa

// Exercise 1.44
// The idea of smoothing a function is an important concept in signal processing. If f is a function and dx is some small number, then the smoothed version of f is the function whose value as a point x is the average of f(x - dx, f(x), and f(x + dx). 

// Write a procedure smooth that takes as input a procedure that computes f and returns a procedure that computes the smoothed f.

func smooth(f: (Double) -> Double) -> (Double) -> Double {
    let dx = 0.001
    return { (x: Double) -> Double in return (f(x - dx) + f(x) + f(x + dx)) / 3 }
}

// It is sometimes valuable to repeatedly smooth a function (that is, smooth the smoothed function) to obtain the n-fold smoothed function. Show how to generate the n-fold smoothed function of any given function using smooth and repeated from exersize 1.43

func compose(f: (Double) -> Double, g: (Double) -> Double) -> (Double) -> Double {
    return { (x: Double) -> Double in return f(g(x)) }
}

func repeated(f: (Double) -> Double , n: Int) -> (Double) -> Double {
    var iter:((Double) -> Double, Int) -> (Double) -> Double = { _ in return { _ in return 0.0 }}
    iter = { (g: (Double) -> Double, step: Int) -> (Double) -> Double in
        if (step == 1) {
            return g
        } else {
            return iter(compose(f, g), step - 1)
        }
    }
    return iter(f, n)
}




/*
func compose(f: ((Double) -> Double) -> (Double) -> Double, g: (Double) -> Double) -> ((Double) -> Double) -> (Double) {
    return { ((x: Double) -> Double) -> (Double) -> Double in return f(g(x)) }
}

func repeated(f: ((Double) -> Double) -> (Double) -> Double , n: Int) -> (Double) -> Double {
    var iter:((Double) -> Double, Int) -> (Double) -> Double = { _ in return { _ in return 0.0 }}
    iter = { (g: (Double) -> Double, step: Int) -> (Double) -> Double in
        if (step == 1) {
            return g
        } else {
            return iter(compose(f, g), step - 1)
        }
    }
    return iter(f, n)
}
*/
func nFoldSmooth(f: (Double) -> Double, n: Int) -> (Double) -> Double {
    var iter: ((Double) -> Double, Int) -> (Double) -> Double = { _, _ in
        { _ in 0.0 } }
    iter = { (g: (Double) -> Double, step:Int) in
        if (step == 0) {
            return g
        } else {
            return iter(smooth(g), step - 1)
        }
    }
    return iter(f, n)
}

sin(0.5)
smooth(sin)(0.5)
nFoldSmooth(sin, 1)(0.5)
nFoldSmooth(sin, 5)(0.5)


// Need to revisit repeated and compose to see if I can make them work for nFold smooth



