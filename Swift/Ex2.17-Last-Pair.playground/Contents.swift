import Cocoa

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

// Exercise 2.17
// Define a procedure lastPair() that returns the list that contains only the last element of a given (nonempty) list:

func lastPair<A>(list: [A]) -> A {
    if cdr(list).isEmpty {
        return car(list)
    } else {
        return lastPair(cdr(list))
    }
}

lastPair([23, 72, 149, 34])
