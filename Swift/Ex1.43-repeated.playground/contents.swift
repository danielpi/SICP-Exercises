import Cocoa

// Exercise 1.43
// If f is a numerical function and n is a positive integer, then we can form the nth repeated application of f, which is degined to be the function whose value at x is f(f(...(f(x))...)). 

// For example,
// If f is the function x -> x + 1, then the nth repeated application of f is the function x -> x + n.
// If f is the operation of squaring a number, then the nth repeated appliation of f is the function that raises its argument to the 2^nth power. 

// Write a procedure that takes as inputs 
// - a procedure that computes f and 
// - a positive integer n 
// and returns the procedure that computes the nth repeated application of f. Your procedure should be able to be used as follows: repeated(square, 2)(5) = 625. You may find it convenient to use compose from exercise 1.42.


func inc(x: Double) -> Double { return x + 1 }
func square(x: Double) -> Double { return x * x }

func compose<T>(f: (T) -> T, g: (T) -> T) -> (T) -> T {
    return { (x: T) -> T in return f(g(x)) }
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
repeated(square, 3)(5)
repeated(inc, 9)(1)



