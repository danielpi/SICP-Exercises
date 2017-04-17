import Cocoa

// Ex 2.31
// Abstract your answer to Exercise 2.30 to produce a procedure tree-map with the property that square-tree could be defined as

// func squareTree(tree: Tree<Int>) -> Tree<Int> {
//      return treeMap(tree, square)
// }

indirect enum Tree<T> {
    case Leaf(T)
    case Node([Tree<T>])
    
    var stringRepresentation: String {
        switch self {
        case let .Leaf(value):
            return " \(value)"
        case let .Node(values):
            let strings = values.map{ $0.stringRepresentation }
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

func square(_ x: Int) -> Int {
    return x * x
}

func treeMap(_ tree: Tree<Int>, _ f:@escaping (Int) -> Int) -> Tree<Int> {
    switch tree {
    case .Leaf(let value):
        return Tree.leaf(f(value))
    case .Node(let values):
        return Tree.Node(values.map{ treeMap($0, f) })
    }
}

func squareTree(_ tree: Tree<Int>) -> Tree<Int> {
    return treeMap(tree, square)
}


let test = Tree.node(Tree.leaf(1), Tree.node(Tree.leaf(2), Tree.list(3,4), Tree.leaf(5)), Tree.list(6, 7))
test.stringRepresentation


let result = squareTree(test)
result.stringRepresentation


