import Cocoa

// Exercise 1.42
// Let f and g be two one-argument functions. The composition f after g is defined to be the function x -> f(g(x)). Define a procedure compose that implements composition. For example, if inc is a procedure that adds 1 to its argument, compose(square, inc)(6) == 49.

func square(x: Double) -> Double { return x * x }
func inc(x: Double) -> Double { return x + 1 }

func compose<T>(f: (T) -> T, _ g: (T) -> T) -> (T) -> T {
    return { (x: T) -> T in return f(g(x)) }
}

compose(square, inc)(6)
square(inc(6))
