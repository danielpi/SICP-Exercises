import Cocoa

//: ### Exercise 2.72
//: Consider the encoding procedure that you designed in Exercise 2.68. What is the order of growth in the number of steps needed to encode a symbol? Be sure to include the number of steps needed to search the symbol list at each node encountered. To answer this question in general is difficult. Consider the special case where the relative frequencies of the n symbols are as described in Exercise 2.71, and give the order of growth (as a function of n) of the number of steps needed to encode the most frequent and least frequent symbols in the alphabet.

/*

func encodeSymbol(symbol: String, tree: Tree) -> [Int] {
    switch tree {
    case let .Leaf(symbol: symbol, weight: w):
        return []
    case let .Branch(left: left, right: right, symbols: syms, weight: _):
        if contains(syms, symbol) {
            if contains(symbols(left.unbox), symbol) {
                return [0] + encodeSymbol(symbol, left.unbox)
            } else {
                return [1] + encodeSymbol(symbol, right.unbox)
            }
        } else {
            fatalError("The symbol:(\(symbol)) is not contained in the tree:(\(tree))")
        }
    }
}

*/

// The steps in encode-symbol are
//  1 Branch and check if a leaf, O(1)
//  2 contains O(n) then + an recurse
//  3 contains again

// So in the specific case of an unbalanced tree the order of growth for the most frequent symbols will be O(n) as you just have to search element-of-set? once.

// The least frequent though requries that you search every branch
//  O(n) + O(n-1) + O(n-2) + ... + O(1)
// The series above would compute out to (n^2)/2 which would be O(n^2) complexity.

