import Cocoa

//: ## Exercise 2.59
//: Implement the unionSet operation for the unordered-list representation of sets.

extension Array {
    var match: (head: T, tail: [T])? {
        return (count > 0) ? (self[0],Array(self[1..<count])) : nil
    }
}

func isElementOfSet<T: Equatable>(x: T, set: [T]) -> Bool {
    if let (head, tail) = set.match {
        if head == x {
            return true
        } else {
            return isElementOfSet(x, tail)
        }
    } else {
        return false
    }
}


func adjoinSet<T: Equatable>(x: T, set: [T]) -> [T] {
    if isElementOfSet(x, set) {
        return set
    } else {
        return [x] + set
    }
}


func intersectionSet<T: Equatable>(set1: [T], set2: [T]) -> [T] {
    if let (head, tail) = set1.match {
        if isElementOfSet(head, set2) {
            return [head] + intersectionSet(tail, set2)
        } else {
            return intersectionSet(tail, set2)
        }
    } else {
        return []
    }
}

func unionSet<T: Equatable>(set1: [T], set2: [T]) -> [T] {
    if let (head, tail) = set1.match {
        if isElementOfSet(head, set2) {
            return unionSet(tail, set2)
        } else {
            return [head] + unionSet(tail, set2)
        }
    } else {
        return set2
    }
}

let set1 = [1,2,4,5]
let set2 = [7,6,5,3,4]

isElementOfSet(3, set1)
adjoinSet(3, set1)
intersectionSet(set1, set2)
unionSet(set1, set2)

