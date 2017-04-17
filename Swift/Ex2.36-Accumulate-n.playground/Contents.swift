import Cocoa

// Exercise 2.35
// The procedure accumulate-n is similar to accumulate except that it takes as its third argument a sequence of sequences, which are all assumed to have the same number of elements. It applies the designated accumulation procedure to combine all the first elements of the sequences, all the second elements of the sequences, and so on, and returns a sequence of the results. 
// For instance, if s is a sequence containing four sequences, ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s) should be the sequence (22 26 30). Fill in the missing expressions in the following definition of accumulate-n

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
func accumulate<A>(_ op: (A, A) -> A, initial: A, seq sequence: [A]) -> A {
    if sequence.isEmpty {
        return initial
    } else {
        return op(car(sequence), accumulate(op, initial: initial, seq: cdr(sequence)))
    }
}

func accumulateN(_ op: (Int, Int) -> Int, initial: Int, seq sequence: [[Int]]) -> [Int] {
    if car(sequence).isEmpty {
        return []
    } else {
        return cons(accumulate(op, initial: initial, seq: sequence.map(car)), accumulateN(op, initial:initial, seq:sequence.map(cdr)))
    }
}

let s = [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]
accumulateN(+, initial:0, seq:s)

