import Foundation

public class Box<T> {
    let unbox: T
    init(_ value: T) {
        self.unbox = value
    }
}

public enum Tree {
    case Leaf(symbol: String, weight: Int)
    case Branch(left: Box<Tree>, right: Box<Tree>, symbols: [String], weight: Int)
}

public func makeLeaf(symbol: String, weight: Int) -> Tree {
    return Tree.Leaf(symbol: symbol, weight: weight)
}

func isLeaf(object: Tree) -> Bool {
    switch object {
    case .Leaf(symbol: _, weight: _):
        return true
    default:
        return false
    }
}

func symbol(x: Tree) -> String {
    switch x {
    case let .Leaf(symbol: s, weight: _):
        return s
    default:
        fatalError("symbol failed \(x)")
    }
}

public func makeCodeTree(left: Tree, right: Tree) -> Tree {
    switch (left, right) {
    case let (.Leaf(symbol:s1, weight:w1), .Leaf(symbol:s2, weight:w2)):
        return Tree.Branch(left: Box(left), right: Box(right), symbols: [s1] + [s2], weight: w1 + w2)
    case let (.Leaf(symbol:s1, weight:w1), .Branch(left: l2, right: r2, symbols:s2, weight:w2)):
        return Tree.Branch(left: Box(left), right: Box(right), symbols: [s1] + s2, weight: w1 + w2)
    case let (.Branch(left: l1, right: r1, symbols:s1, weight:w1), .Leaf(symbol:s2, weight:w2)):
        return Tree.Branch(left: Box(left), right: Box(right), symbols: s1 + [s2], weight: w1 + w2)
    case let (.Branch(left: l1, right: r1, symbols:s1, weight:w1), .Branch(left: l2, right: r2, symbols:s2, weight:w2)):
        return Tree.Branch(left: Box(left), right: Box(right), symbols: s1 + s2, weight: w1 + w2)
        
    }
}

public func leftBranch(tree: Tree) -> Tree {
    switch tree {
    case let .Branch(left: left, right: _, symbols: _, weight: _):
        return left.unbox
    default:
        fatalError("leftBranch failed \(tree)")
    }
}

public func rightBranch(tree: Tree) -> Tree {
    switch tree {
    case let .Branch(left: _, right: right, symbols: _, weight: _):
        return right.unbox
    default:
        fatalError("rightBranch failed \(tree)")
    }
}

func symbols(tree: Tree) -> [String] {
    switch tree {
    case let .Leaf(symbol: symbol, weight: _):
        return [symbol]
    case let .Branch(left: _, right: _, symbols: symbols, weight: _):
        return symbols
    }
}

func weight(tree: Tree) -> Int {
    switch tree {
    case let .Leaf(symbol: _, weight: w1):
        return w1
    case let .Branch(left: _, right: _, symbols: _, weight: w):
        return w
    }
}