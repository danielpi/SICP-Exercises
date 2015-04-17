import Cocoa

// Exercise 2.18
// Define a procedure reverse that takes a list as argument and returns a list of the same elements in revers order

func reverse<A>(list: List<A>) -> List<A> {
    var reverseIter: (List<A>, List<A>) -> List<A> = { _, _ in return List() }
    reverseIter = { input, output in
        if input.isEmpty() {
            return output
        } else {
            return reverseIter(cdr(input)!, cons(car(input)!, output))
        }
    }
    return reverseIter(list, [])
}

reverse([1, 4, 9, 16, 25])
