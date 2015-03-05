import Cocoa

// Exercise 1.41
// Define a procedure double that takes a procedure of one argument as argument and returns a procedure that applies the original procedure twice. For example, if inc is a procedure that adds 1 to its argument, then double(inc) should be a procedure that adds 2. 

func inc(x: Int) -> Int {
    return x + 1
}

func double(f: (Int) -> Int) -> (Int) -> Int {
    return { (x: Int) -> Int in return f(f(x)) }
}

inc(5)
double(inc)(5)

// What value is returned by double(double(double))(inc)(5)

double(double)

// Can't seem to work out how to do this one.


