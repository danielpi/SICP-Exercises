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

// Exercise 2.18
// Define a procedure reverse that takes a list as argument and returns a list of the same elements in revers order

func reverse<A>(list: [A]) -> [A] {
    var reverseIter: ([A], [A]) -> [A] = { _, _ in return [] }
    reverseIter = { input, output in
        if input.isEmpty {
            return output
        } else {
            return reverseIter(cdr(input), cons(car(input), output))
        }
    }
    return reverseIter(list, [])
}

reverse([1, 4, 9, 16, 25])
