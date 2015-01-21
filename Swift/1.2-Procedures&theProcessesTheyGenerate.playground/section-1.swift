// Playground - noun: a place where people can play

import Darwin


// 1.2 Procedures and the Processes they Generate
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
// Fibonacci numbers 0, 1, 1, 2, 3, 5, 8, 13, 21

/*         / 0                       if n = 0
Fib(n) = | 1                       if n = 1
\ Fib(n - 1) + Fib(n - 2) otherwise
*/

func fib(n: Int) -> Int {
    switch true {
    case n == 0:
        return 0
    case n == 1:
        return 1
    default:
        return fib(n - 1) + fib(n - 2)
    }
}
fib(6)

// The fib procedure above involves a lot of redundant computation. It can be shown that the procedure will calculate fib 1 and fib 0 fib(n + 1) times. This is an exponential growth with respect to n. Memory grows linearly with the input.

// We can also process Fibonacci numbers in an iterative process.
// a = a + b
// b = a

func fib1(n: Int) -> Int {
    var fibIter: (Int, Int, Int) -> Int = { _ in return 0 }
    fibIter = { a, b, count in
        if (count == 0) {
            return b
        } else {
            return fibIter(a + b, a, count - 1)
        }
    }
    return fibIter(1, 0, n)
}

//fib1(64)  // Above fib1(91) Swift has a runtime error as the result is larger than 63 bits in size.
// Swift playgrounds aren't fast at this operation either. Dr Racket is significantly faster.


// Example: Counting change
// How many different ways can we make change of $1.00, given half-dollars, quarters, dimes, nickels and pennies? More generally, can we write a procedure to computer the number of ways to change any given amount of money?

// Recursive
// The number of ways to change amount a using n kinds of coins equals
// - the number of ways to change amount a using all but the first kind of coin, plus
// - the number of ways to change amount a - d using all n kinds of coins, where d is the denomination of the first kind of coin.

func countChange(amount: Int) -> Int {
    return cc(amount, 5)
}
func cc(amount: Int, kindsOfCoins: Int) -> Int {
    switch true {
    case amount == 0:
        return 1
    case (amount < 0) || (kindsOfCoins == 0):
        return 0
    default:
        return cc(amount, kindsOfCoins - 1) + cc(amount - firstDenomination(kindsOfCoins), kindsOfCoins)
    }
}
func firstDenomination(kindsOfCoins: Int) -> Int {
    switch kindsOfCoins {
    case 1:
        return 1
    case 2:
        return 5
    case 3:
        return 10
    case 4:
        return 25
    case 5:
        return 50
    default:
        return 0
    }
}
countChange(25)

// The recursive approach used above is very inefficient. Can you come up with an iterative approach that has better performance?


// Exercise 1.11 - A function is defined by the rule that f(n) = n if n < 3 and f(n) = f(n -1) + 2f(n-2) + 3f(n - 3) if n >=3. Write a procedure that computes f by means of a recursive process.
/*
f(n) = | n                                if n  < 3
| f(n - 1) + 2f(n - 2) + 3f(n -3)  if n >= 3
*/

func function11(n: Int) -> Int {
    if (n < 3) {
        return n
    } else {
        return function11(n - 1) + 2*function11(n - 2) + 3*function11(n - 3)
    }
}
function11(8)

// Write a procedure that computes f by means of an iterative process

func function11Iter(three: Int, two: Int, one: Int, count: Int, total: Int) -> Int {
    if (count > total) {
        return one
    } else {
        return function11Iter(two, one, one + 2*two + 3*three, count + 1, total)
    }
}

func function11v2(n: Int) -> Int {
    if (n < 3) {
        return n
    } else {
        return function11Iter( 0, 1, 2, 3, n)
    }
}

function11v2(10)

// Exercise 1.12 - Pascal's Triangle
// The following pattern of numbers is called Pascal's triangle
/*

1
1 1
1 2 1
1 3 3 1
1 4 6 4 1


*/
// The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers above it. Write a procedure that computes elements of Pascal's triangle by means of a recursive process.

// In other words, compute an entry on the Pascal triangle given the row and column. Rows start from 1, counting from above; columns start from 1 too, counting from left to right.

func pascalRecursive(row: Int, col: Int) -> Int {
    switch true {
    case col < 1 || row < 1 || col > row:
        return 0
    case col == row:
        return 1
    case col == 1:
        return 1
    default:
        return pascalRecursive(row - 1, col - 1) + pascalRecursive(row - 1, col)
    }
}
pascalRecursive(1, 1)
pascalRecursive(2, 1)
pascalRecursive(2, 2)
pascalRecursive(3, 1)
pascalRecursive(3, 2)
pascalRecursive(3, 3)
pascalRecursive(10, 5)


// Exercise 1.14

/*
func countChange(amount: Int) -> Int {
return cc(amount, 5)
}
func cc(amount: Int, kindsOfCoins: Int) -> Int {
switch true {
case amount == 0:
return 1
case (amount < 0) || (kindsOfCoins == 0):
return 0
default:
return cc(amount, kindsOfCoins - 1) + cc(amount - firstDenomination(kindsOfCoins), kindsOfCoins)
}
}
func firstDenomination(kindsOfCoins: Int) -> Int {
switch kindsOfCoins {
case 1:
return 1
case 2:
return 5
case 3:
return 10
case 4:
return 25
case 5:
return 50
default:
return 0
}
}
countChange(25)
*/



// Exercise 1.15
// The sine of an angle (specified in radians) can be computed by making use of the approximation sin x ~ x if x is sufficiently small, and the trigonometric identity

// sin r = 3 sin (r/3) - 4 sin^3 (r/3)

// to reduce the size of the argument of sin. (For purposes of this exercise an angle is considered "sufficiently small" if its magnitude is not greater than 0.1 radians. These ideas are incorporated in the following procedures:

func cube(x: Float) -> Float {
    return x * x * x
}
func p(x: Float) -> Float {
    return (3 * x) - (4 * cube(x))
}
func sine(angle: Float) -> Float {
    if (abs(angle) < 0.1) {
        return angle
    } else {
        return p(sine(angle / 3.0))
    }
}

// a) How many times is the procedure p2 applied when (sine 12.15) is evaluated?
// 12.15 / 3^n < 0.1
// 0.1 * 3^n = 12.15
// 3^n = 121.5
// n = 5
sine(12.15)

// b) What is the order of growth in space and number of steps (as a function of a) used by the process generated by the sine procedure when (sine a) is evaluated?

// The number of steps required to calculate sine a can be calculated as below
//        (ceiling(/ (log (/ a 0.1)) (log 3)))
// Which gives us an order of growth for size and space as
//        O(log(n))


// 1.2.4 Exponentiation
// Consider the problem of computing the exponential of a given number. We would like a procedure that takes as arguments a base b and a positive integer exponent n and computes b^n. One way to do this is via the recursive definition
/*
   b^n = b.b^(n-1)
   b^0 = 1
*/

// Which translates into the procedure
func expt1(b: Int, n:Int) -> Int {
    if (n == 0) {
        return 1
    } else {
        return b * expt1(b, n - 1)
    }
}
expt1(2, 10)

// This is a linear recursive process, which requires O(n) steps and space. Just as with factorial we can formulate an equivilent linear iteration

func exptIter(b:Int, count:Int, product: Int) -> Int {
    if (count == 0) {
        return product
    } else {
        
        return exptIter(b, count - 1, b * product)
    }
}
func expt2(b: Int, n:Int) -> Int {
    return exptIter(b, n, 1)
}
expt2(2, 10)

// The version above requires O(n) steps and O(1) space.

// We can compute exponentials in fewer steps by using successive squaring. For instance rather than computing b^8 as
//
// b * b * b * b * b * b * b * b
//
// We can compute it using three multiplications
/*
   b^2 = b * b
   b^4 = b^2 * b^2
   b^8 = b^4 * b^4
*/
// This method works fine for exponents that are powers of 2. We can also take advantage of successive squaring in computing exponentials in genearl if we use the rule
/*
    b^n = (b^2/2)^2     if n is even
    b^n = b*b^(n-1)     if n is odd
*/

func fastExpt(b: Int, n: Int) -> Int {
    switch true {
    case n == 0:
        return 1
    case isEven(n):
        return square(fastExpt(b, n / 2))
    default:
        return b * fastExpt(b, n - 1)
    }
}
func isEven(n: Int) -> Bool {
    return (n % 2) == 0
}
func square(x: Int) -> Int {
    return x * x
}
fastExpt(2, 10)

// fastExpt() grows logarithmically with n in both space and time. To see this observe that computing b^2n requires only one more multiplication than computing b^n. The size of the exponent we can computer therefor doubles with every new multiplication. Hence O(log(n)



// Exercise 1.16
// Design a procedure that evolves an iterative exponentiation process that uses successive squaring and uses a logarithmic number of steps, as does fast-expt.

// Hint: Using the observation that

// (b^(n/2))^2 = (b^2)^(n/2)

// keep, along with
// - the exponent n
// - the base b
// - an additional state variable a,

// and define the state transformation in such a way that the product a.b^n is unchanged from state to state.

// At the beginning of the process a is taken to be 1, and the answer is given by the value of a at the end of the process.

// In general, the technique of defining an invariant quantity that remains unchanged from state to state is a powerful way to thing about the design of iterative algorithms.

func fastExptIter(a: Int, b: Int, n: Int) -> Int {
    println("\(a), \(b), \(n), \(a * fastExpt(b, n))")
    switch true {
    case n == 0:
        return a
    case isEven(n):
        return fastExptIter(a, square(b), n / 2)
    default:
        return fastExptIter(a * b, b, n - 1)
    }
}
func fastExpt2(b: Int, n: Int) -> Int {
    return fastExptIter(1, b, n)
}
fastExpt2(2, 20)


// Exercise 1.17
// Using addition, double and halve design a multiplication procedure analoguous to fast-expt that uses a logarithmic number of steps.

func double(x: Int) -> Int {
    return x + x
}
func halve(x: Int) -> Int {
    return x / 2
}

func fastMultiplyIter(a: Int, b: Int, c: Int) -> Int {
    switch true {
    case b == 1:
        return a + c
    case isEven(b):
        return fastMultiplyIter(double(a), halve(b), c)
    default:
        return fastMultiplyIter(a, b - 1, c + a)
    }
}
func fastMultiply(a: Int, b: Int) -> Int {
    return fastMultiplyIter(a, b, 0)
}
//fastMultiply(2, 4)
fastMultiply(3, 1000000)

