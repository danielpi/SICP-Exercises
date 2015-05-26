import Cocoa

// Exercise 2.26
// Suppose we define x and y to be two lists:

let x = [1, 2, 3]
let y = [4, 5, 6]

// What result is printed by the interpreter in response to evaluating each of the following expressiong

// (append x y) ; (1 2 3 4 5 6)
// (cons x y)   ; ((1 2 3) 4 5 6)
// (list x y)   ; ((1 2 3) (4 5 6))

let append = x + y
// let cons = // Can't do this one as Swift arrays must contain elements of the same type 
let list: [[Int]] = [x,y]

