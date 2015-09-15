import Cocoa

// Exercise 1.44
// The idea of smoothing a function is an important concept in signal processing. If f is a function and dx is some small number, then the smoothed version of f is the function whose value as a point x is the average of f(x - dx, f(x), and f(x + dx). 

// Write a procedure smooth that takes as input a procedure that computes f and returns a procedure that computes the smoothed f.

func smooth(f: (Double) -> Double) -> (Double) -> Double {
    let dx = 0.001
    return { (x: Double) -> Double in return (f(x - dx) + f(x) + f(x + dx)) / 3 }
}

// It is sometimes valuable to repeatedly smooth a function (that is, smooth the smoothed function) to obtain the n-fold smoothed function. Show how to generate the n-fold smoothed function of any given function using smooth and repeated from exersize 1.43

func compose<T>(f: (T) -> T, g: (T) -> T) -> (T) -> T {
    return { (x: T) -> T in return f(g(x)) }
}

func repeatIter<T>(f: (T) -> T, g: (T) -> T, step: Int) -> (T) -> T {
    if (step == 1) {
        return g
    } else {
        return repeatIter(f, g: compose(f, g: g), step: step - 1)
    }
}

func repeated<T>(f: (T) -> T , n: Int) -> (T) -> T {
    return repeatIter(f, g: f, step: n)
}

repeated(smooth, n: 3)(sin)(0.5)


func nFoldSmooth(f: (Double) -> Double, n: Int) -> (Double) -> Double {
    return repeated(smooth, n: n)(f)
}

sin(0.5)
smooth(sin)(0.5)
nFoldSmooth(sin, n: 1)(0.