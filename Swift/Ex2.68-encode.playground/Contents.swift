import Cocoa

//: Exercise 2.68
//: The encode procedure takes as arguments a message and a tree and produces the list of bits that gives the encoded message.

extension Array {
    var match: (head: Element, tail: [T])? {
        return (count > 0) ? (self[0], Array(self[1..<count])) : nil
    }
}

//: encodeSymbol is a procedure, which you must write, that returns the list of bits that encodes a given symbol according to a given tree. You should design encodeSymbol so that it signals an error if the symbol in not in the tree at all. Test your procedure by encoding the result you obtained in Exercise 2.67 with the sample tree and seeing whether it is the same as the original sample message.



func encodeSymbol(symbol: String, tree: Tree) -> [Int] {
    switch tree {
    case let .Leaf(symbol: symbol, weight: w):
        return []
    case let .Branch(left: left, right: right, symbols: syms, weight: _):
        if syms.contains(symbol) {
            if symbols(left.unbox).contains(symbol) {
                return [0] + encodeSymbol(symbol, tree: left.unbox)
            } else {
                return [1] + encodeSymbol(symbol, tree: right.unbox)
            }
        } else {
            fatalError("The symbol:(\(symbol)) is not contained in the tree:(\(tree))")
        }
    }
}

func encode(message:[String], tree:Tree) -> [Int] {
    if let (head, tail) = message.match {
        return encodeSymbol(head, tree) + encode(tail, tree)
    } else {
        return []
    }
}

let sampleTree = makeCodeTree(makeLeaf("A", weight: 4), right: makeCodeTree(makeLeaf("B", weight: 2), right: makeCodeTree(makeLeaf("D", weight: 1), right: makeLeaf("C", weight: 1))))
let sampleMessage = ["A","D","A","B","B","C","A"]
let sampleEncod