import Cocoa

// Exercise 2.27
// Modify your reverse procedure of Exercise 2.18 to produce a deep-reverse procedure that takes a list as argument and returns as its value the list with its elements reversed and with all sublists deep-reversed as well. For example

let x = [[1,2],[3,4]]

extension Array {
    var decompose: (head: Element, tail: [Element])? {
        return (count > 0) ? (self[0], Array(self[1..<count])) : nil
    }
}

func car<A>(_ list:[A]) -> A {
    return list[0]
}
func cdr<A>(_ list:[A]) -> [A] {
    return Array(list[1..<list.count])
}

func reverse<A>(_ list: [A]) -> [A] {
    func reverseIter(_ input: [A], _ output: [A]) -> [A] {
        if input.isEmpty {
            return output
        } else {
            return reverseIter(cdr(input), [car(input)] + output)
        }
    }
    return reverseIter(list, [])
}

reverse(x)

// I can't see how to do this in a general case. The nesting of types in Swift is a bit of a mystery to me (maybe not possible?
func deepReverse<A>(_ list: [[A]]) -> [[A]] {
    var reverseIter: ([[A]], [[A]]) -> [[A]] = { _, _ in return [] }
    reverseIter = { input, output in
        if input.isEmpty {
            return output
        } else {
            return reverseIter(cdr(input), [reverse(car(input))] + output)
        }
    }
    return reverseIter(list, [])
}

deepReverse(x)
