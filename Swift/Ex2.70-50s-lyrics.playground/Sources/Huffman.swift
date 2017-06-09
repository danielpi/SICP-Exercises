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

public func symbols(_ tree: Tree) -> [String] {
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

func chooseBranch(_ bit: Int, _ branch: Tree) -> Tree {
    switch bit {
    case 0:
        return leftBranch(branch)
    case 1:
        return rightBranch(branch)
    default:
        fatalError("chooseBranch failed \(bit)")
    }
}

extension Array {
    var match: (head: Element, tail: [Element])? {
        return (count > 0) ? (self[0], Array(self[1..<count])) : nil
    }
}

public func decode(_ bits: [Int], _ tree: Tree) -> [String] {
    func decode1(_ bits1: [Int], _ currentBranch: Tree) -> [String] {
        if let (_, tail) = bits1.match {
            let nextBranch = chooseBranch(bits1[0], currentBranch)
            switch nextBranch {
            case let .Leaf(symbol: s, weight: _):
                return [s] + decode1(tail, tree)
            default:
                return decode1(tail, nextBranch)
            }
        } else {
            return []
        }
    }
    return decode1(bits, tree)
}

public func encodeSymbol(_ symbol: String, _ tree: Tree) -> [Int] {
    switch tree {
    case .Leaf(symbol: _, weight: _):
        return []
    case let .Branch(left: left, right: right, symbols: syms, weight: _):
        if syms.contains(symbol) {
            if symbols(left).contains(symbol) {
                return [0] + encodeSymbol(symbol, left)
            } else {
                return [1] + encodeSymbol(symbol, right)
            }
        } else {
            fatalError("The symbol:(\(symbol)) is not contained in the tree:(\(tree))")
        }
    }
}

public func encode(_ message:[String], _ tree:Tree) -> [Int] {
    if let (head, tail) = message.match {
        return encodeSymbol(head, tree) + encode(tail, tree)
    } else {
        return []
    }
}


public func adjoinSet(_ x: Tree, _ set: [Tree]) -> [Tree] {
    if let (head, tail) = set.match {
        if weight(x) < weight(head) {
            return [x] + set
        } else {
            return [head] + adjoinSet(x, tail)
        }
    } else {
        return [x]
    }
}

public typealias SymFreqPair = (String, Int)

public func makeLeafSet(_ pairs: [SymFreqPair]) -> [Tree] {
    var result: [Tree] = []
    for pair in pairs {
        result = adjoinSet(makeLeaf(pair.0, pair.1), result)
    }
    return result
}

public func successiveMerge(_ leafSet: [Tree]) -> [Tree] {
    if leafSet.count <= 1 {
        return leafSet
    } else {
        return successiveMerge(adjoinSet(makeCodeTree(leafSet[0], leafSet[1]), Array(leafSet[2..<leafSet.count])))
    }
}

public func generateHuffmanTree(_ pairs: [SymFreqPair]) -> Tree {
    return successiveMerge(makeLeafSet(pairs))[0]
}

