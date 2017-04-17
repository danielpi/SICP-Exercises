import Cocoa

// Exercise 2.36
// Redefine count-leaves from Section 2.2.2 as an accumulation

func cons<A>(_ value: A, _ list: [A]) -> [A] {
    var newList = list
    newList.insert(value, at: 0)
    return newList
}
func car<A>(_ list:[A]) -> A {
    return list[0]
}
func cdr<A>(_ list:[A]) -> [A] {
    return Array(list[1..<list.count])
}
func accumulate<A>(_ op: (A, A) -> A, initial: A, seq sequence: [A]) -> A {
    if sequence.isEmpty {
        return initial
    } else {
        return op(car(sequence), accumulate(op, initial: initial, seq: cdr(sequence)))
    }
}

enum Tree<T> {
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
        let leaves = values.map { Tree.Leaf($0) }
        return Tree.Node(leaves)
    }
}

func enumerateTree(_ tree: Tree<Int>) -> [Int] {
    switch tree {
    case .Leaf(let value):
        return [value]
    case .Node(let values):
        return values.reduce([]) { $0 + enumerateTree($1) }
    }
}

func countLeaves(_ tree: Tree<Int>) -> Int {
    return accumulate({ $1 + 1 }, initial:0, seq:enumerateTree(tree))
}


let b = Tree.node(Tree.leaf(11), Tree.node(Tree.leaf(2), Tree.list(3,4), Tree.leaf(5), Tree.list(6,15)))

countLeaves(b)


func countLeaves2(_ tree: Tree<Int>) -> Int {
    return enumerateTree(tree).reduce(0) { (tally, _) in tally + 1 }
}
countLeaves2(b)







