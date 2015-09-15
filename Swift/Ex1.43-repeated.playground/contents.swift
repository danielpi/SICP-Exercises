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
repeated(square, n: 2)(5)