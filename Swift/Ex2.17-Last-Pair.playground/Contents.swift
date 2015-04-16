import Cocoa

// Exercise 2.17
// Define a procedure lastPair() that returns the list that contains only the last element of a given (nonempty) list:

func lastPair<A>(list: List<A>) -> A {
    if cdr(list)!.isEmpty() {
        return car(list)!
    } else {
        return lastPair(cdr(list)!)
    }
}

lastPair([23, 72, 149, 34])

