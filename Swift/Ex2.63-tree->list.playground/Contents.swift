import Cocoa

//: ## Exercise 2.63
//: Each of the following two procedures converts a binary tree to a list
//: 1. Do the two procedures produce the same result for every tree? If not, how do the results differ? What lists do the two procedures produce for the trees in Figure 2.16?
//: 2. Do the two procedures have the same order of growth in the number of steps required to convert a balanced tree with n elements to a list? If not, which one grows more slowly?


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

func adjoinSet<T: Comparable>(x: T, set: TreeSet<T>) -> TreeSet<T> {
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


// a

func treeToList1<T>(tree: TreeSet<T>) -> [T] {
    switch tree {
    case .Empty:
        return []
    case let .Tree(entry, left, right):
        return treeToList1(left.unbox) + [entry.unbox] + treeToList1(right.unbox)
    }
}

let f = adjoinSet(7, adjoinSet(6, adjoinSet(5, adjoinSet(4, adjoinSet(4, adjoinSet(2, adjoinSet(1, .Empty)))))))

func treeToList2<T>(tree: TreeSet<T>) -> [T] {
    var copyToList: (TreeSet<T>, [T]) -> [T] = { _, _ in return [] }
    copyToList =  { (tree, resultList) in
        switch tree {
        case .Empty:
            return resultList
        case let .Tree(entry, left, right):
            return copyToList(left.unbox, [entry.unbox] + copyToList(right.unbox, resultList))
        }
    }
    return copyToList(tree, [])
}

let fig216a = adjoinSet(11, adjoinSet(9, adjoinSet(5, adjoinSet(1, adjoinSet(3, adjoinSet(7, .Empty))))))
let fig216b = adjoinSet(11, adjoinSet(9, adjoinSet(5, adjoinSet(7, adjoinSet(1, adjoinSet(3, .Empty))))))
let fig216c = adjoinSet(11, adjoinSet(7, adjoinSet(9, adjoinSet(1, adjoinSet(3, adjoinSet(5, .Empty))))))

println("fig216a:\(fig216a)")
println("t->l1 a:\(treeToList1(fig216a))")
println("t->l2 a:\(treeToList2(fig216a))")
println("fig216b:\(fig216b)")
println("t->l1 b:\(treeToList1(fig216b))")
println("t->l2 b:\(treeToList2(fig216b))")
println("fig216c:\(fig216c)")
println("t->l1 c:\(treeToList1(fig216c))")
println("t->l2 c:\(treeToList2(fig216c))")

// To look at the complexity I wanted to be able to use larger sets. So I wrote a couple of functions that allow me to make larger random sets easily.

func adjoinRandom(set: TreeSet<Int>) -> TreeSet<Int> {
    return adjoinSet(Int(arc4random_uniform(100)), set)
}

func adjoinRandomValues(n: Int, set: TreeSet<Int>) -> TreeSet<Int> {
    if n < 1 {
        return set
    } else {
        return adjoinRandomValues(n - 1, adjoinRandom(set))
    }
}
let g = adjoinRandomValues(100,f)
print(g)

println(treeToList1(g))
println(treeToList2(g))


// 1. Yes, both procedure produces the same lists.
// 2. They both have O(n) complexity in Swift.

