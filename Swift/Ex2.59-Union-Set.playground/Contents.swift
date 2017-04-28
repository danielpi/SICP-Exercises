import Cocoa

//: ## Exercise 2.59
//: Implement the unionSet operation for the unordered-list representation of sets.

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
    if isElement(x, ofSet:set) {
        return set
    } else {
        return [x] + set
    }
}


func intersection<T: Equatable>(_ set1: [T], _ set2: [T]) -> [T] {
    if let (head, tail) = set1.match {
        if isElement(head, ofSet: set2) {
            return [head] + intersection(tail, set2)
        } else {
            return intersection(tail, set2)
        }
    } else {
        return []
    }
}

func union<T: Equatable>(_ set1: [T], _ set2: [T]) -> [T] {
    if let (head, tail) = set1.match {
        if isElement(head, ofSet: set2) {
            return union(tail, set2)
        } else {
            return [head] + union(tail, set2)
        }
    } else {
        return set2
    }
}

let set1 = [1,2,4,5]
let set2 = [7,6,5,3,4]

isElement(3, ofSet:set1)
adjoin(3, set1)
intersection(set1, set2)
union(set1, set2)

