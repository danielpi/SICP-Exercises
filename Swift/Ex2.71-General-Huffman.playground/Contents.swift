import Cocoa

//: ## Exercise 2.71
//: Suppose we have a Huffman tree for an alphabet of n symbols, and that the relative frequencies of the symbols are 1,2,4,...,2^n-1. Sketch the tree for n = 5; 

let nFivePairs = [("A",1),("B",2),("C",4),("D",8),("E",16)]
let nFiveTree = generateHuffmanTree(nFivePairs)
print(nFiveTree)

// {a b c d e} 31
//   /       \
// e 16  {a b c d} 15
//        /     \
//      d 8  {a b c} 7
//           /     \
//         c 4   {a b} 3
//                /  \
//              a 1  b 2

//: for n = 10. 

let nTenPairs = nFivePairs + [("F",32),("G",64),("H",128),("I",256),("J",512)]
let nTenTree = generateHuffmanTree(nTenPairs)
print("")
print(nTenTree)

//: In such a tree (for general n) how many bits are required to encode the most frequent symbol?
//:
//: **Answer:** 1
//:
//: The least frequenct symbol?
//:
//: **Answer:** n - 1
