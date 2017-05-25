import Cocoa

//: Exercise 2.67
//: Define an encoding tree and a sample message

let sampleTree = makeCodeTree(makeLeaf("A", 4), makeCodeTree(makeLeaf("B", 2), makeCodeTree(makeLeaf("D", 1), makeLeaf("C", 1))))
let sampleMessage = [0,1,1,0,0,1,0,1,0,1,1,1,0]

//: Use the decode procedure to decode the message, and give the result

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

func decode(_ bits: [Int], _ tree: Tree) -> [String] {
    var decode1: ([Int], Tree) -> [String] = { _, _ in return [] }
    decode1 = { bits1, currentBranch in
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

decode(sampleMessage, sampleTree)


