import Cocoa

//: ## Exercise 2.69
//: The following procedure takes as its argument a list of symbol-frequency pairs (where no symbol appears in more than one pair) and generates a Huffman encoding tree according to the Huffman algorithm.

//: makeLeafSet is the procedure given in section 2.3.4 that transforms the list of pairs into an ordered set of leaves. successiveMerge is the procedure you must write, using makeCodeTree to successively merge the smallest-weight elements of the set until there is only one element left, which is the desired Huffman tree. (This procedure is slightly tricky, but not really complicated. If you find yourself designing a complex procedure, then you are almost certainly doing something wrong. You can take significant advantage of the fact that we are using an ordered set representation.)

extension Array {
    var match: (head: Element, tail: [T])? {
        return (count > 0) ? (self[0], Array(self[1..<count])) : nil
    }
}

typealias SymFreqPair = (String, Int)

func makeLeafSet(pairs: [SymFreqPair]) -> [Tree] {
    var result: [Tree] = []
    for pair in pairs {
        result = adjoinSet(makeLeaf(pair.0, weight: pair.1), set: result)
    }
    return result
}

func successiveMerge(leafSet: [Tree]) -> [Tree] {
    if leafSet.count <= 1 {
        return leafSet
    } else {
        return successiveMerge(adjoinSet(makeCodeTree(leafSet[0], right: leafSet[1]), set: Array(leafSet[2..<leafSet.count])))
    }
}

func generateHuffmanTree(pairs: [SymFreqPair]) -> Tree {
    return successiveMerge(makeLeafSet(pairs))[0]
}


let sampleTree = makeCodeTree(makeLeaf("A", weight: 4), right: makeCodeTree(makeLeaf("B", weight: 2), right: makeCodeTree(makeLeaf("D", weight: 1), right: makeLeaf("C", weight: 1))))
let samplePairs: [SymFreqPair] = [("A",4),("B",2),("C",1),("D",1)]
let sampleLeafSet = makeLeafSet(samplePairs)
print("\(sampleLeafSet)")

let 