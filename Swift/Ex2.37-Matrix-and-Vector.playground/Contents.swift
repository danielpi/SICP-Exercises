import Cocoa

// Exercise 2.37
// Suppose we represent vectors v = (vi) as sequences of numbers, and matricies m = (mij) as sequences of vectors (the rows of the matrix). For example the matrix
/*
 1 2 3 4
 4 5 6 6
 6 7 8 9
*/

// is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this representation, we can use sequence operations to concisely express the basic matrix and vector operations. These operations (which are described in any book on matrix algebra) are the following:

func dotProduct(v: [Int], w: [Int]) -> Int {
    return reduce(map(zip(v,w), *), 0, +)
}
let a = [1,2,3]
let b = [5,6,7]
dotProduct(a, b)

func matrixXVector(m: [[Int]], v: [Int]) -> [Int] {
    return map(m) { dotProduct($0, v) }
}

let c = [[1,0,0],[0,1,0],[0,0,1]]
matrixXVector(c, a)


func cons<A>(value: A, list: [A]) -> [A] {
    var newList = list
    newList.insert(value, atIndex: 0)
    return newList
}
func car<A>(list:[A]) -> A {
    return list[0]
}
func cdr<A>(list:[A]) -> [A] {
    return Array(list[1..<list.count])
}

cons(4, a)
cons(a, c)


func accumulate<A,B>(op: (A, [A]) -> [A], initial: [A], sequence: [A]) -> [A] {
    if sequence.isEmpty {
        return initial
    } else {
        return op(car(sequence), accumulate(op, initial, cdr(sequence)))
    }
}

cons(car(c), c)
//let d: [Int] = accumulate(cons, 1, c)


/*
func accumulateN(op: (Int, [Int]) -> [Int], initial: Int, sequence: [[Int]]) -> [[Int]] {
    if car(sequence).isEmpty {
        return []
    } else {
        return cons(accumulate(op, initial, map(sequence, car)), accumulateN(op, initial, map(sequence, cdr)))
    }
}

func transpose(m: [[Int]]) -> [[Int]] {
    return accumulateN(cons, 0, m)
}
*/





