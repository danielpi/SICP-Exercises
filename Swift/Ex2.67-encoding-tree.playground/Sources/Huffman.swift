import Foundation

public indirect enum Tree {
    case Leaf(symbol: String, weight: Int)
    case Branch(left: Tree, right: Tree, symbols: [String], weight: Int)
}

public func makeLeaf(_ symbol: String, _ weight: Int) -> Tree {
    return Tree.Leaf(symbol: symbol, weight: weight)
}

func isLeaf(_ object: Tree) -> Bool {
    switch object {
    case .Leaf(symbol: _, weight: _):
        return true
    default:
        return false
    }
}

func symbol(_ x: Tree) -> String {
    switch x {
    case let .Leaf(symbol: s, weight: _):
        return s
    default:
        fatalError("symbol failed \(x)")
    }
}

public func makeCodeTree(_ left: Tree, _ right: Tree) -> Tree {
    switch (left, right) {
    case let (.Leaf(symbol:s1, weight:w1), .Leaf(symbol:s2, weight:w2)):
        return Tree.Branch(left: left, right: right, symbols: [s1] + [s2], weight: w1 + w2)
    case let (.Leaf(symbol:s1, weight:w1), .Branch(left: _, right: _, symbols:s2, weight:w2)):
        return Tree.Branch(left: left, right: right, symbols: [s1] + s2, weight: w1 + w2)
    case let (.Branch(left: _, right: _, symbols:s1, weight:w1), .Leaf(symbol:s2, weight:w2)):
        return Tree.Branch(left: left, right: right, symbols: s1 + [s2], weight: w1 + w2)
    case let (.Branch(left: _, right: _, symbols:s1, weight:w1), .Branch(left: _, right: _, symbols:s2, weight:w2)):
        return Tree.Branch(left: left, right: right, symbols: s1 + s2, weight: w1 + w2)
        
    }
}

public func leftBranch(_ tree: Tree) -> Tree {
    switch tree {
    case let .Branch(left: left, right: _, symbols: _, weight: _):
        return left
    default:
        fatalError("leftBranch failed \(tree)")
    }
}

public func rightBranch(_ tree: Tree) -> Tree {
    switch tree {
    case let .Branch(left: _, right: right, symbols: _, weight: _):
        return right
    default:
        fatalError("rightBranch failed \(tree)")
    }
}

func symbols(_ tree: Tree) -> [String] {
    switch tree {
    case let .Leaf(symbol: symbol, weight: _):
        return [symbol]
    case let .Branch(left: _, right: _, symbols: symbols, weight: _):
        return symbols
    }
}

func weight(_ tree: Tree) -> Int {
    switch tree {
    case let .Leaf(symbol: _, weight: w1):
        return w1
    case let .Branch(left: _, right: _, symbols: _, weight: w):
        return w
    }
}
