import Cocoa

// Exercise 2.30
// Define a procedure square-tree analogous to the square-list procedure of Exercise 2.21.


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
            let strings = values.map { $0.unbox.stringRepresentation }
            return "\(strings)"
        }
    }
    
    static func leaf(value: T) -> Tree<T> {
        return Tree.Leaf(Box(value))
    }
    static func node(leaves: Tree<T>...) -> Tree<T> {
        let boxed = leaves.map { Box($0) }
        return Tree.Node(boxed)
    }
    static func list(values: T...) -> Tree<T> {
        let boxedValues = values.map { Box($0) }
        let leaves = boxedValues.map { Tree.Leaf($0) }
        let boxed = leaves.map { Box($0) }
        return Tree.Node(boxed)
    }
}

let test = Tree.node(Tree.leaf(1), Tree.node(Tree.leaf(2), Tree.list(3,4), Tree.leaf(5)), Tree.list(6, 7))
test.stringRepresentation


func squareTree1(tree: Tree<Int>) -> Tree<Int> {
    switch tree {
    case .Leaf(let value):
        return Tree.leaf(value.unbox * value.unbox)
    case .Node(let values):
        return Tree.Node( values.map { Box(squareTree1($0.unbox)) })
    }
}
let result = squareTree1(test)
result.stringRepresentation

