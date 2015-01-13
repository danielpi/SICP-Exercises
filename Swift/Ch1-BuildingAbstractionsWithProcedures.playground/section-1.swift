// Chapter 1 - Building Abstrations with Procedures

import Darwin

// Exercise 1.1
10
5 + 3 + 4
9 - 1
6 / 2
(2 * 4) + (4 - 6)
let a = 3
let b = a + 1
(a * b) + a + b
a == b
if ((b > a) && (b < (a * b))) {
    b
} else {
    a
}
switch 4 {
case a:
    6
case b:
    6 + 7 + a
default:
    25
}
// Alternative to above
switch true {
case a == 4:
    6
case b == 4:
    6 + 7 + a
default:
    25
}
((b > a) ? b : a) + 2
switch true {
case a > b:
    a * (a + 1)
case a < b:
    b * (a + 1)
default:
    -1 * (a + 1)
}


// Exercise 1.2
// let inOneGo = (5 + 4 + (2 - (3 - (6 + (4.0 / 5))))) / (3.0 * (6 - 2) * (2 - 7)) // Fails due to the calculation taking too long!!! That is pretty ordinary performance on Swift's part.
let numerator = (5 + 4 + (2 - (3 - (6 + (4.0 / 5)))))
let denominator: Double = (3 * (6 - 2) * (2 - 7))
let answer = numerator / denominator


// Exercise 1.3 - Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.
func square(x: Int) -> Int {
    return x * x
}
func sumOfSquares(x: Int, y: Int) -> Int {
    return square(x) + square(y)
}

func sumOfSquaresOfTwoLargest(a: Int, b: Int, c: Int) -> Int {
    switch true {
    case min(a, b, c) == a:
        return sumOfSquares(b, c)
    case min(a, b, c) == b:
        return sumOfSquares(a, c)
    case min(a, b, c) == c:
        return sumOfSquares(a, b)
    default:
        println("Something went badly wrong with the sumOfSquaresOfTwoLargest() function")
        return 0
    }
}
sumOfSquaresOfTwoLargest(6, 2, 4)


// Exercise 1.4 - Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behaviou of the following procedure
// (define (a-plus-abs-b a b) ((if (> b 0) + -) a b))
func aPlusABSb(a: Int, b: Int) -> Int {
    if (b > 0) {
        return a + b
    } else {
        return a - b
    }
}
aPlusABSb(4, 5)
aPlusABSb(4, -5)
// Swift doesn't really have the same ability as lisp in this regard. In the function above two entire blocks of code which use different operators can be selected between but the operator of a particular procedure is not being set at runtime.


// Exercise 1.5 - Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:
/*
func p() -> () {
    return p()
}

func test(x:Int, y: () -> ()) -> Int {
    if (x == 0) {
        return 0
    } else {
        return y
    }
}
I don't think this one makes much sense in Swift. I can't see how we can have a function that returns a closure or number for a start and also the test doesn't really dig into how swift works (as best I can understand.
*/

func square(x: Double) -> Double {
    return x * x
}

// 1.1.7 Example: Square Roots by Newton's Method
// Functions are declarative knowledge and procedures are imperative knowledge. Meaning that functions describe functions of things and procedures describe how to do things.

// How does one compute square roots?
// Guess a value, y, for the value of the square root of a number x. Find the quotient of x divided by the guess. Average the quotient and the guess. Continue till you have an accurate enough answer.

func sqrtIter(guess: Double, x: Double) -> Double {
    if (goodEnough(guess, x)) {
        return guess
    } else {
        return sqrtIter(improve(guess, x), x)
    }
}

func improve(guess: Double, x: Double) -> Double {
    return average(guess, (x / guess))
}
func average(x: Double, y: Double) -> Double {
    return ((x + y) / 2)
}
func goodEnough(guess: Double, x: Double) -> Bool {
    return abs((guess * guess) - x) < 0.001
}
func DRPsqrt(x: Double) -> Double {
    return sqrtIter(1.0, x)
}

DRPsqrt(9)
DRPsqrt(100 + 37)
DRPsqrt(DRPsqrt(2) + DRPsqrt(3))
square(sqrt(1000))


// Exercise 1.6 - Alyssa P. Hacker doesn't see why if needs to be provided as a special form. " Why can't I just define it as an ordinary procedure in terms of cond?" she asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if:

func newIf(predicate: Bool, thenClause: Double, elseClause: Double) -> Double {
    switch true {
    case predicate:
        return thenClause
    default:
        return elseClause
    }
}

newIf((2 == 3), 0, 5)
newIf((1 == 1), 0, 5)

func newSqrtIter(guess: Double, x: Double) -> Double {
    return newIf(goodEnough(guess, x), guess, newSqrtIter(improve(guess, x), x))
}
//newSqrtIter(1.0, 9)
// The above causes an nfinit loop in the Swift interpreter.


// Exercise 1.7 - The goodEnough() test used in computing square roots will not be very effective for finding the square roots of very small numbers. Also in real computers, arithmetic operations are almost always performed with limited precision. This makes our test inadequate for very large numbers. Explain these statements, with examples showing how the test fails for small and large numbers.

// Small numbers
DRPsqrt(0.001)
// goodEnough() uses a fixed comparision of 0.001 for the square of the guess to the value x no matter what size x is. If x is small (similar in size to 0.001) then the goodEnough result will be true when the result is not very accurate.

// Large numbers
DRPsqrt(1000000000000)
//sqrt(10000000000000) // This line crashes XCode


func sqrtIter2(prevGuess: Double, guess: Double, x: Double) -> Double {
    if (goodEnough2(prevGuess, guess)) {
        return guess
    } else {
        return sqrtIter2(guess, improve(guess, x), x)
    }
}

func goodEnough2(prevGuess: Double, guess: Double) -> Bool {
    return (abs(prevGuess - guess) / guess) < 0.001
}
func sqrt2(x: Double) -> Double {
    return sqrtIter2(0.0, 1.0, x)
}
sqrt2(9)
sqrt2(0.001)
sqrt2(10000000000000)


// Exercise 1.8 - Newton's method for cube roots is based on the fact that if y is an approximation to the cube root of x, then a better approximation is given by the value
// x/y^2 + 2y
// ----------
//      3
// Use this formula to implement a cube-root procedure analogous to the square-root procedure.

func cbrtIter(guess: Double, x: Double) -> Double {
    if (goodEnoughCubeRoot(guess, x)) {
        return guess
    } else {
        return cbrtIter(improveCubeRoot(guess, x), x)
    }
}
func cube(x: Double) -> Double {
    return x * x * x
}
func improveCubeRoot(guess: Double, x: Double) -> Double {

    return ((x / (guess * guess)) + (2 * guess)) / 3.0
}
func goodEnoughCubeRoot(guess: Double, x: Double) -> Bool {
    return abs((guess * guess * guess) - x) < 0.0001
}
func DRPcbrt(x: Double) -> Double {
    return cbrtIter(1.0, x)
}

DRPcbrt(8)
DRPcbrt(0.001)
DRPcbrt(100000)


// 1.1.8 Procedures as Black-Box Abstractions
func DRPdouble(x: Double) -> Double {
    return x + x
}
func square3(x: Double) -> Double {
    return exp(DRPdouble(log(x)))
}

square3(4)


// Internal definitions and block structure

func DRPsqrt2(x: Double) -> Double {
    func goodEnough(guess: Double, x: Double) -> Bool {
        return abs((guess * guess) - x) < 0.001
    }
    func average(x: Double, y: Double) -> Double {
        return ((x + y) / 2)
    }
    func improve(guess: Double, x: Double) -> Double {
        return average(guess, (x / guess))
    }
    
    var sqrtIter: (Double, Double) -> (Double) = { _ in return 0.0 }
    sqrtIter = { guess, x in
        if (goodEnough(guess, x)) {
            return guess
        } else {
            return sqrtIter(improve(guess, x), x)
        }
    }
    
    return sqrtIter(1.0, x)
}

DRPsqrt2(100)

/*
func DRPsqrt3(x: Double) -> Double {
    func goodEnough(guess: Double) -> Bool {
        return abs((guess * guess) - x) < 0.001
    }
    func average(a: Double, b: Double) -> Double {
        return ((a + b) / 2)
    }
    func improve(guess: Double) -> Double {
        return average(guess, (x / guess))
    }
    
    var sqrtIter: (Double) -> (Double) = { _ in return 0.0 }
    sqrtIter = { guess in
        if (goodEnough(guess)) {
            return guess
        } else {
            let improved = improve(guess)
            return sqrtIter(improved)
        }
    }
    
    return sqrtIter(1.0)
}

DRPsqrt2(100)

I'm not sure if the above is possible with swift.
*/


// Exer 1.2 Procedures and the Processes they Generate
// 1.2.1 Linear Recursion and Iteration
// Factorial n!
// Factorial can be thought of by noticing that n! == n * (n - 1)! . This is a recursive process

/*
 (factorial 3)
 (* 3 (factorial 2))
 (* 3 (* 2 (factorial 1)))
 (* 3 (* 2 1)))
 (* 3 2)
 6
*/

func factorialRecursive(n: Int) -> Int {
    if (n == 1) {
        return 1
    } else {
        return n * factorialRecursive(n - 1)
    }
}
factorialRecursive(6)

// Note the ramped appearence of the calculation. This is due to the fact that the interpreter must keep track of a large amount of state as it progresses ???

// Lets take a different approach. We could maintain a running product along with a counter that counts from 1 to n

// product = counter * product
// counter = counter + a

/*
 (factorial 3)
 (fact-iter 1 1 3)
 (fact-iter 1 2 3)
 (fact-iter 2 3 3)
 6
*/

func factIter(product: Int, counter: Int, maxCount: Int) -> Int {
    if (counter > maxCount) {
        return product
    } else {
        return factIter(product * counter, counter + 1, maxCount)
    }
}
func factorialIterative(n: Int) -> Int {
    return factIter(1, 1, n)
}
factorialIterative(6)

func factorialIterative2(n:Int) -> Int {
    
    var factIter2: (Int, Int, Int) -> Int = { _ in return 0 }
    factIter2 = { product, counter, maxCount in
        if (counter > maxCount) {
            return product
        } else {
            return factIter2(product * counter, counter + 1, maxCount)
        }
    }
    return factIter2(1, 1, n)
}
factorialIterative2(6)

// Both approaches compute the same mathematical function and require the same number of steps, which is proportional to n. The first approach has an expansion and then contraction. The sxpansion is due to a build up of deferred operations. The contraction is when the operations are performed. This is called a linear recursive process.
// The second approach doesn't shrink or grow. At each step all we need to keep track of for any n are the current values of product, counter and max-count. This is a linear iterative process.
// Most popular languages are designed in such a way that the interpretation  of any recursive process consumes an amount of memory that grows with the number of procedure calls. As such special looping constructs are required. Tail recursion can solve this defect though.


// Exercise 1.9 - Each of the following two procedures defines a method for adding two positive integers in terms of the procedures inc, which increments its argument by 1, and dec, which decrements its argument by 1.
func inc(value: Int) -> Int {
    return value + 1
}
func dec(value: Int) -> Int {
    return value - 1
}

func addition1(a: Int, b: Int) -> Int {
    if (a == 0) {
        return b
    } else {
        return inc(addition1(dec(a), b))
    }
}

addition1(4, 5)
/*
inc(addition1(3, 5))
inc(inc(addition1(2, 5)))
inc(inc(inc(addition1(1, 5))))
inc(inc(inc(inc(addition1(0, 5)))))
inc(inc(inc(inc(5))))
inc(inc(inc(6)))
inc(inc(7))
inc(8)
9
*/


func addition2(a: Int, b: Int) -> Int {
    if (a == 0) {
        return b
    } else  {
        return addition2(dec(a), inc(b))
    }
}

addition2(4, 5)
/*
addition2(3, 6)
addition2(2, 7)
addition2(1, 8)
addition2(0, 9)
9
*/
// addition1 is a recursive process. addition2 is an iterative process


// Exercise 1.10 - The following procedure computes a mathematical function called Ackermann's function.
func A(x:Int, y:Int) -> Int {
    println("x:\(x) y:\(y)")
    switch true {
    case y == 0:
        return 0
    case x == 0:
        return 2 * y
    case y == 1:
        return 2
    default:
        return A(x - 1, A(x, y - 1))
    }
}
// What are the values of the following expressions?
A(1, 10)
/*
x:1 y:10
x:1 y:9
x:1 y:8
x:1 y:7
x:1 y:6
x:1 y:5
x:1 y:4
x:1 y:3
x:1 y:2
x:1 y:1
x:0 y:2
x:0 y:4
x:0 y:8
x:0 y:16
x:0 y:32
x:0 y:64
x:0 y:128
x:0 y:256
x:0 y:512
Final output is 1024
*/
A(2, 4)
/*
x:2 y:4
x:2 y:3
x:2 y:2
x:2 y:1
x:1 y:2
x:1 y:1
x:0 y:2
x:1 y:4
x:1 y:3
x:1 y:2
x:1 y:1
x:0 y:2
x:0 y:4
x:0 y:8
x:1 y:16
x:1 y:15
x:1 y:14
x:1 y:13
x:1 y:12
x:1 y:11
x:1 y:10
x:1 y:9
x:1 y:8
x:1 y:7
x:1 y:6
x:1 y:5
x:1 y:4
x:1 y:3
x:1 y:2
x:1 y:1
x:0 y:2
x:0 y:4
x:0 y:8
x:0 y:16
x:0 y:32
x:0 y:64
x:0 y:128
x:0 y:256
x:0 y:512
x:0 y:1024
x:0 y:2048
x:0 y:4096
x:0 y:8192
x:0 y:16384
x:0 y:32768
Final output is 65536
*/
A(3, 3)
/*
x:3 y:3
x:3 y:2
x:3 y:1
x:2 y:2
x:2 y:1
x:1 y:2
x:1 y:1
x:0 y:2
x:2 y:4
x:2 y:3
x:2 y:2
x:2 y:1
x:1 y:2
x:1 y:1
x:0 y:2
x:1 y:4
x:1 y:3
x:1 y:2
x:1 y:1
x:0 y:2
x:0 y:4
x:0 y:8
x:1 y:16
x:1 y:15
x:1 y:14
x:1 y:13
x:1 y:12
x:1 y:11
x:1 y:10
x:1 y:9
x:1 y:8
x:1 y:7
x:1 y:6
x:1 y:5
x:1 y:4
x:1 y:3
x:1 y:2
x:1 y:1
x:0 y:2
x:0 y:4
x:0 y:8
x:0 y:16
x:0 y:32
x:0 y:64
x:0 y:128
x:0 y:256
x:0 y:512
x:0 y:1024
x:0 y:2048
x:0 y:4096
x:0 y:8192
x:0 y:16384
x:0 y:32768
Final output is 65536
*/

func f(n:Int) -> Int {
    return A(0, n)
}
// f(n) = 2n

func g(n:Int) -> Int {
    return A(1, n)
}
// g(n) = 2^n

func h(n: Int) -> Int {
    return A(3, n)
}
// h(n) = 2^(2^n)


// 1.2.2 Tree Recursion

