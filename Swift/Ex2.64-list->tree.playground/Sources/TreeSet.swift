import Foundation

public class Box<T> {
    let unbox: T
    init(_ value: T) {
        self.unbox = value
    }
}

public enum TreeSet<T>: CustomStringConvertible {
    case Empty
    case Tree(entry:Box<T>, left:Box<TreeSet<T>>, right: Box<TreeSet<T>>)
    
    public var description : String {
        switch self {
        case .Empty:
            return "()"
        case let .Tree(entry, left, right):
            return "(\(entry.unbox) \(left.unbox) \(right.unbox))"
        }
    }
}

public func entry<T>(tree: TreeSet<T>) -> T {
    switch tree {
    case let .Tree(entry, _, _):
        return entry.unbox
    default:
        fatalError("Tried to read an entry from an empty tree")
    }
}

public func leftBranch<T>(tree: TreeSet<T>) -> TreeSet<T> {
    switch tree {
    case let .Tree(_, left, _):
        return left.unbox
    default:
        fatalError("Tried to read the left branch from an empty tree")
    }
}

public func rightBranch<T>(tree: TreeSet<T>) -> TreeSet<T> {
    switch tree {
    case let .Tree(_, _, right):
        return right.unbox
    default:
        fatalError("Tried to read the right branch from an empty tree")
    }
}

public func makeTree<T>(entry: T, left:TreeSet<T>, right:TreeSet<T>) -> TreeSet<T> {
    return TreeSet.Tree(entry: Box(entry), left: Box(left), right: Box(right))
}

public func isElementOfSet<T: Comparable>(x: T, set: TreeSet<T>) -> Bool {
    switch set {
    case .Empty:
        return false
    case let .Tree(entry, _, _) where entry.unbox == x:
        return true
    case let .Tree(entry, left, _) where entry.unbox < x:
        return isElementOfSet(x, set: left.unbox)
    case let .Tree(entry, _, right) where entry.unbox > x:
        return isElementOfSet(x, set: right.unbox)
    default:
        fatalError("isElementOfSet3 has an unhandled case when x:\(x) and set:\(set)")
    }
}

public func adjoinSet<T: Comparable>(x: T, set: TreeSet<T>) -> TreeSet<T> {
    switch set {
    case .Empty:
        return makeTree(x, left: .Empty, right: .Empty)
    case let .Tree(entry, _, _) where entry.unbox == x:
        return set
    case let .Tree(entry, left, right) where entry.unbox > x:
        return makeTree(entry.unbox, left: adjoinSet(x, set: left.unbox), right: right.unbox)
    case let .Tree(entry, left, right) where entry.unbox < x:
        return makeTree(entry.unbox, left: left.unbox, right: adjoinSet(x, set: right.unbox))
    default:
        fatalError("adjoinSet3 didn't handle all cases when x:\(x) set:\(set)")
    }
}

public func treeToList<T>(tree: TreeSet<T>) -> [T] {
    switch tree {
    case .Empty:
        return []
    case let .Tree(entry, left, right):
        return treeToList(left.unbox) + [entry.unbox] + treeToList(right.unbox)
    }
}

public func adjoinRandom(set: TreeSet<Int>) -> TreeSet<Int> {
    return adjoinSet(Int(arc4random_uniform(100)), set: set)
}

public func adjoinRandomValues(n: Int, set: TreeSet<Int>) -> TreeSet<Int> {
    if n < 1 {
        return set
    } else {
        return adjoinRandomValues(n - 1, set: adjoinRandom(set))
    }
}

