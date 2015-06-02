import Cocoa

// Exercise 2.38
// The accumulate procedure is also known as fold-right, because it combines the first element of the sequence with the result of combining all the elements to the right. There is also a fold-left, which is similar to fold-right, except that it combines elements working in the opposite direction:

func cons<A>(value: A, list: [A]) -> [A] {
    var newList = list
    newList.insert(value, atIndex: 0)
    return newList
}
func car<A>(list:[A]) -> A {
    return list[0]
}
func cdr<A>(list:[A]) -> [A] {
    return Array(list[1..<list.count])
}


func foldl<A,B>(op: (B, A) -> B, initial: B, sequence:[A]) -> B {
    var iter: (B, [A]) -> B = { (a,_) in return a }
    
    iter = { (result, rest) in
        if rest.isEmpty {
            return result
        } else {
            return iter(op(result, car(rest)), cdr(rest))
        }
    }
    return iter(initial, sequence)
}

func foldr<A,B>(op: (A, B) -> B, initial: B, sequence: [A]) -> B {
    if sequence.isEmpty {
        return initial
    } else {
        return op(car(sequence), foldr(op, initial, cdr(sequence)))
    }
}


foldr(/, 64.0, [2,2,2])
foldl(/, 1.0, [1,2,3])


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
    static func cons(left: T, right: Tree<T>) -> Tree<T> {
        let l = Tree.leaf(left)
        return Tree.node(l, right)
    }
    static func cons(left: Tree<T>, right: T) -> Tree<T> {
        let r = Tree.leaf(right)
        return Tree.node(left, r)
    }
}


let a = foldr(Tree.cons, Tree.Node([]), [1,2,3])
a.stringRepresentation

let b = foldl(Tree.cons, Tree.Node([]), [1,2,3])
b.stringRepresentation


// Give a property that op should satisfy to guarantee that fold-right and fold-left will produce the same values for any sequence

// op needs to obey the commutativity property. This means that the function is the same in one direction as it is in the other.
