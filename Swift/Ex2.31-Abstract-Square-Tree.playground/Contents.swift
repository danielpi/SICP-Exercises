import Cocoa

// Ex 2.31
// Abstract your answer to Exercise 2.30 to produce a procedure tree-map with the property that square-tree could be defined as

// func squareTree(tree: Tree<Int>) -> Tree<Int> {
//      return treeMap(tree, square)
// }



class Box<T>{
    let unbox: T
    init(_ value: T) {
        self.unbox = value
    }
}

enum Tree<T> {
    case Leaf(Box<T>)
    case Node([Box<Tree<T>>])
    
    var stringRepresentation: String {
        switch self {
        case let .Leaf(value):
            return " \(value.unbox)"
        case let .Node(values):
            let strings = map(values) { $0.unbox.stringRepresentation }
            return "\(strings)"
        }
    }
    
    static func leaf(value: T) -> Tree<T> {
        return Tree.Leaf(Box(value))
    }
    static func node(leaves: Tree<T>...) -> Tree<T> {
        let boxed = map(leaves) { Box($0) }
        return Tree.Node(boxed)
    }
    static func list(values: T...) -> Tree<T> {
        let boxedValues = map(values) { Box($0) }
        let leaves = map(boxedValues) { Tree.Leaf($0) }
        let boxed = map(leaves) { Box($0) }
        return Tree.Node(boxed)
    }
}

func square(x: Int) -> Int {
    return x * x
}

func treeMap(tree: Tree<Int>, f: (Int) -> Int) -> Tree<Int> {
    switch tree {
    case .Leaf(let value):
        return Tree.leaf(f(value.unbox))
    case .Node(let values):
        return Tree.Node(map(values) { Box(treeMap($0.unbox, f)) })
    }
}

func squareTree(tree: Tree<Int>) -> Tree<Int> {
    return treeMap(tree, square)
}


let test = Tree.node(Tree.leaf(1), Tree.node(Tree.leaf(2), Tree.list(3,4), Tree.leaf(5)), Tree.list(6, 7))
test.stringRepresentation


let result = squareTree(test)
result.stringRepresentation


