import Cocoa

// Exercise 2.30
// Define a procedure square-tree analogous to the square-list procedure of Exercise 2.21.

indirect enum Tree<T> {
    case Leaf(T)
    case Node([Tree<T>])
    
    var stringRepresentation: String {
        switch self {
        case let .Leaf(value):
            return " \(value)"
        case let .Node(values):
            let strings = values.map { $0.stringRepresentation }
            return "\(strings)"
        }
    }
    
    static func leaf(_ value: T) -> Tree<T> {
        return Tree.Leaf(value)
    }
    static func node(_ leaves: Tree<T>...) -> Tree<T> {
        return Tree.Node(leaves)
    }
    static func list(_ values: T...) -> Tree<T> {
        let leaves = values.map{ Tree.Leaf($0) }
        return Tree.Node(leaves)
    }
}

let test = Tree.node(Tree.leaf(1), Tree.node(Tree.leaf(2), Tree.list(3,4), Tree.leaf(5)), Tree.list(6, 7))
test.stringRepresentation


func squareTree1(_ tree: Tree<Int>) -> Tree<Int> {
    switch tree {
    case .Leaf(let value):
        return Tree.leaf(value * value)
    case .Node(let values):
        return Tree.Node( values.map{ squareTree1($0) })
    }
}
let result = squareTree1(test)
result.stringRepresentation

