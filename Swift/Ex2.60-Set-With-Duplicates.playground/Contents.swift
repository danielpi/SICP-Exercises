import Cocoa

//: ## Exercise 2.60
//: We specified that a set would be represented as a list with no duplicates. Now suppose we allow duplicates. For instance, the set {1,2,3} could be represented as the list [2, 3, 2, 1, 3, 2, 2]. Design procedures isElementOfSet, adjoinSet, unionSet, and intersectionSet that operate on this representation.


extension Array {
    var match: (head: Element, tail: [Element])? {
        return (count > 0) ? (self[0],Array(self[1..<count])) : nil
    }
}

func isElement<T: Equatable>(_ x: T, ofSet set: [T]) -> Bool {
    if let (head, tail) = set.match {
        if head == x {
            return true
        } else {
            return isElement(x, ofSet:tail)
        }
    } else {
        return false
    }
}


func adjoin<T: Equatable>(_ x: T, _ set: [T]) -> [T] {
    return [x] + set
}


func intersection<T: Equatable>(_ set1: [T], _ set2: [T]) -> [T] {
    if let (head, tail) = set1.match {
        if isElement(head, ofSet:set2) {
            return [head] + intersection(tail, set2)
        } else {
            return intersection(tail, set2)
        }
    } else {
        return []
    }
}

func union<T: Equatable>(_ set1: [T], _ set2: [T]) -> [T] {
    return set1 + set2
}

let set1 = [1,2,4,5]
let set2 = [7,6,5,3,4]

isElement(3, ofSet: set1)
adjoin(3, set1)
intersection(set1, set2)
union(set1, set2)

//: How does the efficiency of each compare with the corresponding procedure for the non-duplicate representation?
//: - adjoinSet becomes very cheap
//: - as does unionSet
//: - isElementOfSet is the same procedure however now there may be significantly more values to search for due to the duplicates.
//: - intersectionSet is the same as isElementOfSet due to the additions of the duplicates to the size of n.

//: Are there applications for which you would use this representation in preference to the non-duplicate one?
//: 
//: Applications where it is important to be able to add to the set very quickly but for which retreival time, or memory size are not important.

