import Cocoa

// Exercise 2.33
// Fill in the missing expressiongs to complete the following definitions of some basic list-manipulation operations as accumulations:

func map(p: (Int) -> Int, _ sequence: [Int]) -> [Int] {
    return sequence.reduce([]) { $0 + [p($1)] }
}

func square(x: Int) -> Int { return x * x }
map(square, [1,2,3,4,5])


func cons<A>(value: A, _ list: [A]) -> [A] {
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
func append(seq1: [Int], _ seq2: [Int]) -> [Int] {
    return seq1.reverse().reduce(seq2) { cons($1, $0) }
}
append([1,2,3,4], [5,6,7,8])


func length(sequence: [Int]) -> Int {
    return sequence.reduce(0) { $0 + $1 }
}
length([1,2,3,4,5,7])

