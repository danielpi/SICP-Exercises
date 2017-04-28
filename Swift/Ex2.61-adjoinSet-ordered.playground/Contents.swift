import Cocoa

//: ## Exercise 2.61
//: Give an implementation of adjoinSet using the ordered representation. By analogy with isElementOfSet show how to take advantage of the ordering to produce a procedure that requires on the average about half as many steps as with the unordered representation.

extension Array {
    var match: (head: Element, tail:[Element])? {
        return (self.isEmpty) ? nil : (self[0], Array(self[1..<self.count]))
    }
}

let orderedSet = [1,3,5,7,9]
orderedSet.match

func adjoin<T: Comparable>(_ x: T, _ set: [T]) -> [T] {
    if let (head, tail) = set.match {
        switch true {
        case x == head:
            return set
        case x < head:
            return [x] + set
        default:
            return [head] + adjoin(x, tail)
        }
    } else {
        return [x]
    }
}

adjoin(4, [])
adjoin(4, orderedSet)
adjoin(5, orderedSet)

