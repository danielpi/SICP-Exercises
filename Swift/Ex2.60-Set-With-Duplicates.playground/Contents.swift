import Cocoa

//: ## Exercise 2.60
//: We specified that a set would be represented as a list with no duplicates. Now suppose we allow duplicates. For instance, the set {1,2,3{ could be represented as the list [2, 3, 2, 1, 3, 2, 2]. Design procedures isElementOfSet, adjoinSet, unionSet, and intersectionSet that operate on this representation. 


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
    return [x] + set
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
    return set1 + set2
}

let set1 = [1,2,4,5]
let set2 = [7,6,5,3,4]

isElementOfSet(3, set1)
adjoinSet(3, set1)
intersectionSet(set1, set2)
unionSet(set1, set2)

//: How does the efficiency of each compare with the corresponding procedure for the non-duplicate representation?
//: - adjoinSet becomes very cheap
//: - as does unionSet
//: - isElementOfSet is the same procedure however now there may be significantly more values to search for due to the duplicates.
//: - intersectionSet is the same as isElementOfSet due to the additions of the duplicates to the size of n.

//: Are there applications for which you would use this representation in preference to the non-duplicate one?
//: 
//: Applications where it is important to be able to add to the set very quickly but for which retreival time, or memory size are not important.

