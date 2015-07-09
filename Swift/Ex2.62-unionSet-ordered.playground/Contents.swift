import Cocoa

//: ## Exercise 2.62
//: Give a O(n) implementation of unionSet for sets represented as ordered lists.

extension Array {
    var match: (head: T, tail: [T])? {
        return self.isEmpty ? nil : (self[0], Array(self[1..<self.count]))
    }
}

func unionSet<T: Comparable>(set1: [T], set2: [T]) -> [T] {
    switch (set1.match, set2.match) {
    case (.None, .None):
        return []
    case (.None, _):
        return set2
    case (_, .None):
        return set1
    case (.Some(let head1, let tail1), .Some(let head2, let tail2)):
        switch true {
        case head1 == head2:
            return [head1] + unionSet(tail1, tail2)
        case head1 > head2:
            return [head2] + unionSet(set1, tail2)
        case head1 < head2:
            return [head1] + unionSet(tail1, set2)
        default:
            fatalError("unionSet failed evaluating head1:\(head1) head2:\(head2)")
        }
    default:
        fatalError("unionSet failed evaluating set1:\(set1) set2:\(set2)")
    }
}

let orderedSet1 = [1,3,5,7,9]
let orderedSet2 = [2,4,6,8,10]
unionSet([Int](), [Int]())
unionSet([], orderedSet2)
unionSet(orderedSet1, [])
unionSet(orderedSet1, orderedSet2)
unionSet(orderedSet1, [1,2,3,4,5,6,7,8,9,10])
