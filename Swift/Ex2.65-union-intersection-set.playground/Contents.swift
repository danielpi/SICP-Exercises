import Cocoa

//: ## Exercise 2.65
//: Use the results of Exercise 2.63 and Exercise 2.64 to give O(n) implementations of unionSet and intersectionSet for sets implemented as (balanced binary trees.

class Box<T> {
    let unbox: T
    init(_ value: T) {
        self.unbox = value
    }
}

typealias TreeSetList = (TreeSet<Int>,[Int])

enum TreeSet<T>: CustomStringConvertible {
    case Empty
    case Tree(entry:Box<T>, left:Box<TreeSet<T>>, right: Box<TreeSet<T>>)
    
    var description : String {
        switch self {
        case .Empty:
            return "()"
        case let .Tree(entry, left, right):
            return "(\(entry.unbox) \(left.unbox) \(right.unbox))"
        }
    }
}

func entry<T>(tree: TreeSet<T>) -> T {
    switch tree {
    case let .Tree(entry, _, _):
        return entry.unbox
    default:
        fatalError("Tried to read an entry from an empty tree")
    }
}

func leftBranch<T>(tree: TreeSet<T>) -> TreeSet<T> {
    switch tree {
    case let .Tree(_, left, _):
        return left.unbox
    default:
        fatalError("Tried to read the left branch from an empty tree")
    }
}

func rightBranch<T>(tree: TreeSet<T>) -> TreeSet<T> {
    switch tree {
    case let .Tree(_, _, right):
        return right.unbox
    default:
        fatalError("Tried to read the right branch from an empty tree")
    }
}

func makeTree<T>(entry: T, _ left:TreeSet<T>, _ right:TreeSet<T>) -> TreeSet<T> {
    return TreeSet.Tree(entry: Box(entry), left: Box(left), right: Box(right))
}


func isElementOfSet<T: Comparable>(x: T, _ set: TreeSet<T>) -> Bool {
    switch set {
    case .Empty:
        return false
    case let .Tree(entry, _, _) where entry.unbox == x:
        return true
    case let .Tree(entry, left, _) where entry.unbox < x:
        return isElementOfSet(x, left.unbox)
    case let .Tree(entry, _, right) where entry.unbox > x:
        return isElementOfSet(x, right.unbox)
    default:
        fatalError("isElementOfSet3 has an unhandled case when x:\(x) and set:\(set)")
    }
}

func adjoinSet<T: Comparable>(x: T, _ set: TreeSet<T>) -> TreeSet<T> {
    switch set {
    case .Empty:
        return makeTree(x, .Empty, .Empty)
    case let .Tree(entry, _, _) where entry.unbox == x:
        return set
    case let .Tree(entry, left, right) where entry.unbox > x:
        return makeTree(entry.unbox, adjoinSet(x, left.unbox), right.unbox)
    case let .Tree(entry, left, right) where entry.unbox < x:
        return makeTree(entry.unbox, left.unbox, adjoinSet(x, right.unbox))
    default:
        fatalError("adjoinSet3 didn't handle all cases when x:\(x) set:\(set)")
    }
}

func treeToList<T>(tree: TreeSet<T>) -> [T] {
    switch tree {
    case .Empty:
        return []
    case let .Tree(entry, left, right):
        return treeToList(left.unbox) + [entry.unbox] + treeToList(right.unbox)
    }
}

func adjoinRandom(set: TreeSet<Int>) -> TreeSet<Int> {
    return adjoinSet(Int(arc4random_uniform(100)), set)
}

func adjoinRandomValues(n: Int, _ set: TreeSet<Int>) -> TreeSet<Int> {
    if n < 1 {
        return set
    } else {
        return adjoinRandomValues(n - 1, adjoinRandom(set))
    }
}

func partialTree(elts: [Int], _ n: Int) -> TreeSetList {
    if n == 0 {
        return (.Empty, elts)
    } else {
        let leftSize = (n - 1) / 2
        let (leftTree, nonLeftElts) = partialTree(elts, leftSize)
        let rightSize = n - (leftSize + 1)
        let thisEntry = nonLeftElts[0]
        let (rightTree, remainingElts) = partialTree(Array(nonLeftElts[1..<nonLeftElts.count]), rightSize)
        
        return (makeTree(thisEntry, leftTree, rightTree), remainingElts)
    }
}

func listToTree(elements: [Int]) -> TreeSet<Int> {
    let (tree, _) = partialTree(elements, elements.count)
    return tree
}

extension Array {
    var match: (head: Element, tail: [Element])? {
        return self.isEmpty ? nil : (self[0], Array(self[1..<self.count]))
    }
}

func unionOrderedList<T: Comparable>(set1: [T], _ set2: [T]) -> [T] {
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
            return [head1] + unionOrderedList(tail1, tail2)
        case head1 > head2:
            return [head2] + unionOrderedList(set1, tail2)
        case head1 < head2:
            return [head1] + unionOrderedList(tail1, set2)
        default:
            fatalError("unionOrderedList failed evaluating head1:\(head1) head2:\(head2)")
        }
    default:
        fatalError("unionSet failed evaluating set1:\(set1) set2:\(set2)")
    }
}

func unionSet(set1: TreeSet<Int>, _ set2: TreeSet<Int>) -> TreeSet<Int> {
    return listToTree(unionOrderedList(treeToList(set1), treeToList(set2)))
}

let g = adjoinRandomValues(100,.Empty)

let fig216a = adjoinSet(11, adjoinSet(9, adjoinSet(5, adjoinSet(1, adjoinSet(3, adjoinSet(7, .Empty))))))
let fig216b = adjoinSet(11, adjoinSet(9, adjoinSet(5, adjoinSet(7, adjoinSet(1, adjoinSet(3, .Empty))))))
let fig216c = adjoinSet(11, adjoinSet(7, adjoinSet(9, adjoinSet(1, adjoinSet(3, adjoinSet(5, .Empty))))))

let random1 = adjoinRandomValues(10,.Empty)
let random2 = adjoinRandomValues(10,.Empty)
print(treeToList(random1))
print(treeToList(random2))
let union = unionSet(random1, random2)
print(treeToList(union))



func intersectionOrderedList<T: Comparable>(set1: [T], _ set2: [T]) -> [T] {
    if let (x1, tail1) = set1.match,
        (x2, tail2) = set2.match {
            switch true {
            case x1 == x2:
                return [x1] + intersectionOrderedList(tail1, tail2)
            case x1 < x2:
                return intersectionOrderedList(tail1, set2)
            case x1 > x2:
                return intersectionOrderedList(set1, tail2)
            default:
                fatalError("intersectionSet2 failed with values of x1:\(x1) and x2:\(x2)")
            }
    } else {
        return []
    }
}


func intersectionSet(set1: TreeSet<Int>, _ set2: TreeSet<Int>) -> TreeSet<Int> {
    return listToTree(intersectionOrderedList(treeToList(set1), treeToList(set2)))
}

let intersection = intersectionSet(random1, random2)
print(treeToList(intersection))



