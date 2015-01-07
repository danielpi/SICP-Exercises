// Chapter 1 - Building Abstrations with Procedures

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

