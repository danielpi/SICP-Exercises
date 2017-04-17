import Cocoa

// Exercise 2.33
// Fill in the missing expressiongs to complete the following definitions of some basic list-manipulation operations as accumulations:

func map(_ p: (Int) -> Int, _ sequence: [Int]) -> [Int] {
    return sequence.reduce([]) { $0 + [p($1)] }
}

func square(_ x: Int) -> Int { return x * x }
map(square, [1,2,3,4,5])


func cons<A>(_ value: A, _ list: [A]) -> [A] {
    var newList = list
    newList.insert(value, at: 0)
    return newList
}
func car<A>(_ list:[A]) -> A {
    return list[0]
}
func cdr<A>(_ list:[A]) -> [A] {
    return Array(list[1..<list.count])
}
func append(_ seq1: [Int], _ seq2: [Int]) -> [Int] {
    return seq1.reversed().reduce(seq2) { cons($1, $0) }
}
append([1,2,3,4], [5,6,7,8])


func length(_ sequence: [Int]) -> Int {
    return sequence.reduce(0) { $0 + $1 }
}
length([1,2,3,4,5,7])

