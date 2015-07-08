import Cocoa

//: ## Example: Representing Sets
//: In the previous examples we built representations for two kinds of compound data objects: rational numbers and algebraic expressions. In one of these examples we had the choice of simplifying (reducing) the expressions at either construction time or selection time, but other than that the choice of a representation for these structures in terms of lists was straightforward. When we turn to the representation of sets, the choice of a representation is not so obvious. Indeed, there are a number of possible representations, and they differ significantly from one another in several ways.
//:
//: Informally, a set is simply a collection of distinct objects. To give a more precise definition we can employ the method of data abstraction. That is, we define "set" by specifying the operations that are to be used on sets. These are union-set, intersection-set, elements-of-set?, and adjoin-set. element-of-set? is a predicate that determines whether a given element is a member of a set. adjoin-set takes an object and a set as arguments and returns a set that contains the elements of the original set and also the adjoined element. union-set computes the union of two sets, which is the set containing each element that appears in either argument. intersection-set computes the intersection of two sets, which is the set containing only elements that appear in both arguments. From the viewpoint of data abstraction, we are free to design any representation that implements these operations in a way consistent with the interpretations given above.
//:
//: ### Sets as unordered lists
//: One way to represent a set is as a list of its elements in which no element appears more than once. The empty set is represented by the empty list. In this representation, element-of-set? is similar to the procedure memq of Section 2.3.1. It uses equal? instead of eq? so that the set elements need not be symbols:

extension Array {
    var match: (head: T, tail: [T])? {
        return (count > 0) ? (self[0],Array(self[1..<count])) : nil
    }
}

let unorderedSet1 = [10,3,6,1]

func isElementOfSet1<T: Equatable>(x: T, set: [T]) -> Bool {
    if let (head, tail) = set.match {
        if head == x {
            return true
        } else {
            return isElementOfSet1(x, tail)
        }
    } else {
        return false
    }
}
isElementOfSet1(3, unorderedSet1)
isElementOfSet1(7, unorderedSet1)

//: Using this we can write adjoinSet. If the object to be adjoined is already in the set, we just return the set. Otherwise, we use + (cons) to add the object to the list that represents the set:

func adjoinSet1<T: Equatable>(x: T, set: [T]) -> [T] {
    if isElementOfSet1(x, set) {
        return set
    } else {
        return [x] + set
    }
}
let unorderedSet2 = adjoinSet1(8, unorderedSet1)

//: For intersectionSet we can use a recursive strategy. If we know how to form the intersection of set2 and the tail of set1, we only need to decide whether to include the head of set1 in this. But this depends on whether the head of set1 is also in set2. Here is the resulting procedure:

func intersectionSet1<T: Equatable>(set1: [T], set2: [T]) -> [T] {
    if let (head, tail) = set1.match {
        if isElementOfSet1(head, set2) {
            return [head] + intersectionSet1(tail, set2)
        } else {
            return intersectionSet1(tail, set2)
        }
    } else {
        return []
    }
}
intersectionSet1(unorderedSet1, unorderedSet2)

//: In designing a representation, one of the issues we should be concerned with is efficiency. Consider the number of steps required by our set operations. Since they all use isElementOfSet, the speed of this operation has a major impact on the efficiency of the set implementation as a whole. Now, in order to check whether an object is a member of a set, isElementOfSet may have to scan the entire set. (In the worst case, the object turns out not to be in the set.) Hence, if the set has n elements, isElementOfSet might take up to n steps. Thus, the number of steps required grows as O(n). The number of steps required by adjoinSet, which uses this operation, also grows as O(n). For intersectionSet, which does an isElementOfSet check for each element of set1, the number of steps required grows as the product of the sizes of the sets involved, or O(n^2) for two sets of size n. The same will be true of unionSet

//: ### Sets as ordered lists
//: One way to speed up our set operations is to change the representation so that the set elements are listed in increasing order. To do this, we need some way to compare two objects so that we can say which is bigger. For example, we could compare symbols lexicographically, or we could agree on some method for assigning a unique number to an object and the compare the elements by comparing the corresponding numbers. To  keep our discussion simple, we will consider only the case where the set elements are numbers, so that we can compare elements using > and <. We will represent a set of numbers by listing its elements in increasing order. Whereas our first representation above allowed us to represent the set {1,3,6,10} by listing the elements in any order, our new representation allows only the list [1,3,6,10].
//:
//: One advantage of ordering shows up in isElementOfSet: In checking for the presence of an item, we no longer have to scan the entire set. If we reach a set element that is larger than the item we are looking for, then we know that the item is not in the set:

func isElementOfSet2<T: Comparable>(x: T, set: [T]) -> Bool {
    if let (head, tail) = set.match {
        switch head {
        case x:
            return true
        case _ where head > x:
            return false
        default:
            return isElementOfSet2(x, tail)
        }
    } else {
        return false
    }
}
let orderedSet1 = [2,5,7,8,9]
isElementOfSet2(5, orderedSet1)
isElementOfSet2(3, orderedSet1)




