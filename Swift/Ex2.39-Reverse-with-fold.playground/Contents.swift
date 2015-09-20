import Cocoa

// Exercise 2.39
// Complete the following definitions of reverse in terms of fold-right and fold-left


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


func foldl<A,B>(op: (B, A) -> B, initial: B, seq sequence:[A]) -> B {
    var iter: (B, [A]) -> B = { (a,_) in return a }
    
    iter = { (result, rest) in
        if rest.isEmpty {
            return result
        } else {
            return iter(op(result, car(rest)), cdr(rest))
        }
    }
    return iter(initial, sequence)
}

func foldr<A,B>(op: (A, B) -> B, initial: B, seq sequence: [A]) -> B {
    if sequence.isEmpty {
        return initial
    } else {
        return op(car(sequence), foldr(op, initial:initial, seq:cdr(sequence)))
    }
}


func reverse1(sequence: [Int]) -> [Int] {
    return foldr({ (x, y) in y + [x] }, initial:[], seq:sequence)
}
reverse1([1,2,3])


func reverse2(sequence: [Int]) -> [Int] {
    return foldl({ (x, y) in [y] + x }, initial:[], seq:sequence)
}
reverse2([1,2,3])
