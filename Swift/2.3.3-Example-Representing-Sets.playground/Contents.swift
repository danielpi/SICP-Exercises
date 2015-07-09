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

//: How many steps does this save? In the worst case, the item we are looking for may be the largest one in the set, so the number of steps is the same as for the unordered representation. On the other hand, if we search for items of many different sizes we can expect that sometimes we will be able to stop searching at a point near the beginning of the list and that other times we will still need to examine most of the list. On the average we should expect to have to examine about half of the items in the set. Thus, the average number of steps required will be about n/2. This is still O(n) growth, but it does save us, on the average, a factor of 2 in number of steps over the previous implementation.
//:
//: We obtain a more impressive speedup with intersectionSet. In the unordered representation this operation required O(n^2) steps, because we performed a complete scan of set2 for each element of set1. But with the ordered representation, we can use a more clever method. Begin by comparing the initial elements, x1 and x2, of the two sets. If x1 equals x2, then that gives an element of the intersection, and the rest of the intersection is the intersection of the tails of the two sets. Suppose, however, that x1 is less than x2. Since x2 is the smallest element in set2, we can immedietely conclude that x1 cannot appear anywhere in set2 and hence is not in the intersection. Hence, the intersection is equal to the intersection of set2 with the tail of set1. Similarly, if x2 is less than x1, then the intersection is given by the intersection of set1 with the tail of set2. Here is the procedure:

func intersectionSet2<T: Comparable>(set1: [T], set2: [T]) -> [T] {
    if let (x1, tail1) = set1.match,
        (x2, tail2) = set2.match {
            switch true {
            case x1 == x2:
                return [x1] + intersectionSet2(tail1, tail2)
            case x1 < x2:
                return intersectionSet2(tail1, set2)
            case x1 > x2:
                return intersectionSet2(set1, tail2)
            default:
                fatalError("intersectionSet2 failed with values of x1:\(x1) and x2:\(x2)")
            }
    } else {
        return []
    }
}
let orderedSet2 = [1,2,3,4,6,7,9]
intersectionSet2(orderedSet1, orderedSet2)

//: To estimate the number of steps required by this process, observe that at each step we reduce the intersection problem to computing intersections of smaller sets - removing the first element from set1 or set2 or both. Thus, the number of steps required is at most the sum of the sizes of set1 and set2, rather than the product of the sizes as with the unordered representation. This is O(n) growth rather than O(n^2) - a considerable speedup, even for sets of moderate size.

//: ### Sets as binary trees
//: We can do better than the ordered-list representation by arranging the set elements in the form of a tree. Each node of the tree holds one element of the set, called the "entry" at that node, and a link to each of two other (possibly empty) nodes. The "left" link points to elements smaller than the one at the node, and the "right" link to elements greater than the one at the node. Figure 2.16 shows some trees that represent the set {1,3,5,7,9,11}. The only thing we require for a valid representtion is that all elements in the left subtree be smaller than the node entry and that all elements in the right subtree be larger.

let figure216 = NSImage(named: "figure2-16.png")


//: **Figure 2.16:** Various trees that represent the set {1,3,5,7,9,11}. 

//: The advantage of the tree representation is this: Suppose we want to check whether a number x is contained in a set. We begin by comparing x with the entry in the top node. If x is less than this, we know that we need only search the left subtree; if x is greater, we need only search the right subtree. Now, if the tree is "balanced," each of these subtrees will be about half the size of the original. Thus, in one step we have reduced the problem of searching a tree of size n to searching a tree of size n/2. Since the size of the tree is halved at each step, we should expect that the number of steps needed to search a tree of size n grows as O(log n). For large sets, this will be a significant speedup over the previous representations.
//:
//: We can represent trees by using lists. Each node will be a list of three items: the entry at the node, the left subtree, and the right subtree. A left or right subtree of the empty list will indicate that there is no subtree connected there. We can describe this representation by the following procedures;
//:
//: For Swift I'm not even going to attempt the approach listed in the book (of using a list as the data structure). Instead I'll use enums with associated types.

class Box<T> {
    let unbox: T
    init(_ value: T) {
        self.unbox = value
    }
}

enum TreeSet<T>: Printable {
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
    case let .Tree(entry, left, right):
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

func makeTree<T>(entry: T, left:TreeSet<T>, right:TreeSet<T>) -> TreeSet<T> {
    return TreeSet.Tree(entry: Box(entry), left: Box(left), right: Box(right))
}

let a = makeTree(5, .Empty, .Empty)
println(a)
entry(a)
println(leftBranch(a))
println(rightBranch(a))

//: Now we can write the isElementOfSet procedure using the strategy described above:

func isElementOfSet3<T: Comparable>(x: T, set: TreeSet<T>) -> Bool {
    switch set {
    case .Empty:
        return false
    case let .Tree(entry, _, _) where entry.unbox == x:
        return true
    case let .Tree(entry, left, _) where entry.unbox < x:
        return isElementOfSet3(x, left.unbox)
    case let .Tree(entry, _, right) where entry.unbox > x:
        return isElementOfSet3(x, right.unbox)
    default:
        fatalError("isElementOfSet3 has an unhandled case when x:\(x) and set:\(set)")
    }
}
isElementOfSet3(5, a)
isElementOfSet3(6, a)

//: Adjoining an item to a set is implemented similarly and also requires O(log n) steps. To adjoin an item x, we compare x with the node entry to determine whether x should be added to the right or to the left branch, and having adjoined x to the appropriate branch we piece this newly constructed branch together with the original entry and the other branch. If x is equal to the entry, we just return the node. If we are asked to adjoin x to an empty tree, we generate a tree that has x as the entry and empty right and left branches. Here is the procedure:

func adjoinSet3<T: Comparable>(x: T, set: TreeSet<T>) -> TreeSet<T> {
    switch set {
    case .Empty:
        return makeTree(x, .Empty, .Empty)
    case let .Tree(entry, _, _) where entry.unbox == x:
        return set
    case let .Tree(entry, left, right) where entry.unbox < x:
        return makeTree(entry.unbox, adjoinSet3(x, left.unbox), right.unbox)
    case let .Tree(entry, left, right) where entry.unbox > x:
        return makeTree(entry.unbox, left.unbox, adjoinSet3(x, right.unbox))
    default:
        fatalError("adjoinSet3 didn't handle all cases when x:\(x) set:\(set)")
    }
}
let b = adjoinSet3(7, a)
let c = adjoinSet3(3, b)
let d = adjoinSet3(4, c)
let e = adjoinSet3(5, d)
println(e)

//: The above claim that searching the tree can be performed in a logarithmic number of steps rests on the assumption that the tree is "balanced," i.e., that the left and right subtree of every tree have approximately the same number of elements, so that each subtree contains about half the elements of its parent. But how can we be certain that the trees we construct will be balanced? Even if we start with a balanced tree, adding elements with adjoinSet may produce an unbalanced result. Since the position of a newly adjoined element depends on how the element compares with the items already in the set, we can expect that if we add elements "randomly" the tree will tend to be balanced on the average. But this is not a guarantee. For example, if we start with an empty set and adjoin the numbers 1 through 7 in sequence we end up with the highly unbalanced tree shown in Figure 2.17. In this tree all the left subtrees are empty, so it has no advantage over a simple ordered list. One way to solve this problem is to define an operation that transforms an arbitrary tree into a balanced tree with the same elements. Then we can perform this transformation after every few adjoinSet operations to keep our set in balance. There are also other ways to solve this problem, most of which involve designing new data structures for which searching and insertion both can be done in O(log n) steps.

let figure217 = NSImage(named: "figure2-17.png")

let f = adjoinSet3(7, adjoinSet3(6, adjoinSet3(5, adjoinSet3(4, adjoinSet3(3, adjoinSet3(2, adjoinSet3(1, .Empty)))))))
println(f)

//: **Figure 2.17** Unbalanced tree produced by adjoining 1 through 7 in sequence

//: ## Sets and information retrieval


