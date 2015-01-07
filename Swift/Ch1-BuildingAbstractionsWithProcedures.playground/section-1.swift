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


// Exercise 1.3
