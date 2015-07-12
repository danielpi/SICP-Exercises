import Cocoa

//: ## Exercise 2.64
//: The following procedure list->tree converts an ordered list to a balanced binary tree. The helper procedure partial-tree takes as arguments an integer n and list of at least n elements and constructs a balanced tree containing the first n elements of the list. The result returned by partial-tree is a pair (formed with cons) whose car is the constructed tree and whose cdr is the list of elements not included in the tree.


typealias TreeSetList = (TreeSet<Int>,[Int])

func partialTree(elts: [Int], n: Int) -> TreeSetList {
    if n == 0 {
        return (.Empty, elts)
    } else {
        let leftSize = (n - 1) / 2
        let (leftTree, nonLeftElts) = partialTree(elts, leftSize)
        let rightSize = n - (leftSize + 1)
        let thisEntry = nonLeftElts[0]
        let (rightTree, remainingElts) = partialTree(Array(nonLeftElts[1..<nonLeftElts.count]), rightSize)
        
        return (makeTree(thisEntry, leftTree, rightTree), remainingElts)
    }
}

func listToTree(elements: [Int]) -> TreeSet<Int> {
    let (tree, list) = partialTree(elements, elements.count)
    return tree
}

let blah = listToTree([1,3,5,7,9,11])
print(blah)
treeToList(blah)


// a. Write a short paragraph explaining as clearly as you can how partialTree works. Draw the tree produced by listToTree for the list (1 3 5 7 9 11).

//     5
//   /   \
//  1     9
//   \   / \
//    3 7   11

// If n is 0 then we are at a leaf and thus we need an empty tree. Otherwise we recursively grab half of the remaining values and make one branch of the tree with them and make another branch from the other half.

// b. What is the order of growth in the number of steps required by listToTree to convert a list of n elements?

// O(n)

