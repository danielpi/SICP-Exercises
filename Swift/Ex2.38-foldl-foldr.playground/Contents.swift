import Cocoa

// Exercise 2.38
// The accumulate procedure is also known as fold-right, because it combines the first element of the sequence with the result of combining all the elements to the right. There is also a fold-left, which is similar to fold-right, except that it combines elements working in the opposite direction:

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


func foldl<A>(op: (A, A) -> A, initial: A, sequence:[A]) -> A {
    var iter: (A, [A]) -> A = { (a,_) in return a }
    
    iter = { (result, rest) in
        if rest.isEmpty {
            return result
        } else {
            return iter(op(result, car(rest)), cdr(rest))
        }
    }
    return iter(initial, sequence)
}

func foldr<A>(op: (A, A) -> A, initial: A, sequence: [A]) -> A {
    if sequence.isEmpty {
        return initial
    } else {
        return op(car(sequence), foldr(op, initial, cdr(sequence)))
    }
}


foldr(/, 64.0, [2,2,2])
foldl(/, 1.0, [1,2,3])

//foldr(+, [], [1,2,3])


