import Cocoa

//: ## Exercise 2.61
//: Give an implementation of adjoinSet using the ordered representation. By analogy with isElementOfSet show how to take advantage of the ordering to produce a procedure that requires on the average about half as many steps as with the unordered representation.

extension Array {
    var match: (head: T, tail:[T])? {
        return (self.isEmpty) ? nil : (self[0], Array(self[1..<self.count]))
    }
}

let orderedSet = [1,3,5,7,9]
orderedSet.match

func adjoinSet<T: Comparable>(x: T, set: [T]) -> [T] {
    if let (head, tail) = set.match {
        switch true {
        case x == head:
            return set
        case x < head:
            return [x] + set
        default:
            return [head] + adjoinSet(x, tail)
        }
    } else {
        return [x]
    }
}

adjoinSet(4, [])
adjoinSet(4, orderedSet)
adjoinSet(5, orderedSet)

