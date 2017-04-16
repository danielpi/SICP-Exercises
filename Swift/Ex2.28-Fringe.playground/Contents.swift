import Cocoa

// Exercise 2.28
// Write a procedure fringe that takes as argument a tree (represented as a list) and returns a list whose elements are all the leaves of the tree arranged in left-to-right order.

let x = [[1,2],[3,4]]
let xx = [[[1,2],[3,4]],[[1,2],[3,4]]]


func fringe(_ input: [[Int]]) -> [Int] {
    return input.flatMap{ $0 }
}
fringe(x)


func fringe(_ input: [[[Int]]]) -> [Int] {
    return input.flatMap{ $0 }.flatMap{ $0 }
}
fringe(xx)

// Again I don't see how to create a generic version of the finge function. 

func fringely<A>(_ input: [[A]]) -> [A] {
    return input.flatMap{ $0 }
}

fringely(x)
fringely(xx)



