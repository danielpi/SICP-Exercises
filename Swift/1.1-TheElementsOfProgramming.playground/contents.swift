//: # Chapter 1 - Building Abstrations with Procedures
//:
//: > The acts of the mind, wherein it exerts its power over simple ideas, are chiefly these three: 
//: > 1. Combining several simple ideas into one compound one, and thus all complex ideas are made. 
//: > 2. The second is bringing two ideas, whether simple or complex, together, and setting them by one another so as to take a view of them at once, without uniting them into one, by which it gets all its ideas of relations. 
//: > 3. The third is separating them from all other ideas that accombany them in their real existence: this is called abstraction, and thus all its general ideas are made.
//: >
//: > John Locke, An Essay Concerning Human Understanding (1690)
//:
//: We are about to study the idea of a *computational process*. Computational processes are abstract beings that inhabit computers. As they evolve, processes manipulate other abstract things called *data*. The evolution of a process is directed by a pattern of rules called *program*. People create programs to direct processes. In effect, we conjure the spirits of the computer with our spells.
//:
//: 

import Darwin

//: ## Exercise 1.1
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


//: ## Exercise 1.2
//: 
let inOneGo = (5 + 4 + (2 - (3 - (6 + (4.0 / 5))))) / (3.0 * (6 - 2) * (2 - 7))
let numerator = (5 + 4 + (2 - (3 - (6 + (4.0 / 5)))))
let denominator: Double = (3 * (6 - 2) * (2 - 7))
let answer = numerator / denominator


// Exercise 1.3 - Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.
func square(_ x: Int) -> Int {
    return x * x
}
func sumOfSquares(_ x: Int, _ y: Int) -> Int {
    return square(x) + square(y)
}

func sumOfSquaresOfTwoLargest(_ a: Int, _ b: Int, _ c: Int) -> Int {
    switch true {
    case min(a, b, c) == a:
        return sumOfSquares(b, c)
    case min(a, b, c) == b:
        return sumOfSquares(a, c)
    case min(a, b, c) == c:
        return sumOfSquares(a, b)
    default:
        print("Something went badly wrong with the sumOfSquaresOfTwoLargest() function")
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
aPlusABSb(a: 4, b: 5)
aPlusABSb(a: 4, b: -5)
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

func square(_ x: Double) -> Double {
    return x * x
}

// 1.1.7 Example: Square Roots by Newton's Method
// Functions are declarative knowledge and procedures are imperative knowledge. Meaning that functions describe functions of things and procedures describe how to do things.

// How does one compute square roots?
// Guess a value, y, for the value of the square root of a number x. Find the quotient of x divided by the guess. Average the quotient and the guess. Continue till you have an accurate enough answer.

func sqrtIter(_ guess: Double, _ x: Double) -> Double {
    if (goodEnough(guess, x)) {
        return guess
    } else {
        return sqrtIter(improve(guess, x), x)
    }
}

func improve(_ guess: Double, _ x: Double) -> Double {
    return average(guess, (x / guess))
}
func average(_ x: Double, _ y: Double) -> Double {
    return ((x + y) / 2)
}
func goodEnough(_ guess: Double, _ x: Double) -> Bool {
    return abs((guess * guess) - x) < 0.001
}
func DRPsqrt(_ x: Double) -> Double {
    return sqrtIter(1.0, x)
}

DRPsqrt(9)
DRPsqrt(100 + 37)
DRPsqrt(DRPsqrt(2) + DRPsqrt(3))
square(sqrt(1000))


// Exercise 1.6 - Alyssa P. Hacker doesn't see why if needs to be provided as a special form. " Why can't I just define it as an ordinary procedure in terms of cond?" she asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if:

func newIf(_ predicate: Bool, Then: Double, Else: Double) -> Double {
    switch true {
    case predicate:
        return Then
    default:
        return Else
    }
}

newIf((2 == 3), Then: 0, Else: 5)
newIf((1 == 1), Then: 0, Else: 5)

func newSqrtIter(_ guess: Double, _ x: Double) -> Double {
    return newIf(goodEnough(guess, x), Then: guess, Else: newSqrtIter(improve(guess, x), x))
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


func sqrtIter2(_ prevGuess: Double, _ guess: Double, _ x: Double) -> Double {
    if (goodEnough2(prevGuess, guess)) {
        return guess
    } else {
        return sqrtIter2(guess, improve(guess, x), x)
    }
}

func goodEnough2(_ prevGuess: Double, _ guess: Double) -> Bool {
    return (abs(prevGuess - guess) / guess) < 0.001
}
func sqrt2(_ x: Double) -> Double {
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

func cbrtIter(_ guess: Double, _ x: Double) -> Double {
    if (goodEnoughCubeRoot(guess, x)) {
        return guess
    } else {
        return cbrtIter(improveCubeRoot(guess, x), x)
    }
}
func cube(_ x: Double) -> Double {
    return x * x * x
}
func improveCubeRoot(_ guess: Double, _ x: Double) -> Double {

    return ((x / (guess * guess)) + (2 * guess)) / 3.0
}
func goodEnoughCubeRoot(_ guess: Double, _ x: Double) -> Bool {
    return abs((guess * guess * guess) - x) < 0.0001
}
func DRPcbrt(_ x: Double) -> Double {
    return cbrtIter(1.0, x)
}

DRPcbrt(8)
DRPcbrt(0.001)
DRPcbrt(100000)


// 1.1.8 Procedures as Black-Box Abstractions
func DRPdouble(_ x: Double) -> Double {
    return x + x
}
func square3(_ x: Double) -> Double {
    return exp(DRPdouble(log(x)))
}

square3(4)


// Internal definitions and block structure

func DRPsqrt2(_ x: Double) -> Double {
    func goodEnough(_ guess: Double, _ x: Double) -> Bool {
        return abs((guess * guess) - x) < 0.001
    }
    func average(_ x: Double, _ y: Double) -> Double {
        return ((x + y) / 2)
    }
    func improve(_ guess: Double, _ x: Double) -> Double {
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



