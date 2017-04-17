import Cocoa

// Exercise 2.38
// The accumulate procedure is also known as fold-right, because it combines the first element of the sequence with the result of combining all the elements to the right. There is also a fold-left, which is similar to fold-right, except that it combines elements working in the opposite direction:

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


func foldl<A,B>(_ op:@escaping (B, A) -> B, initial: B, seq sequence:[A]) -> B {
    func iter(_ result: B, _ rest: [A]) -> B {
        if rest.isEmpty {
            return result
        } else {
            return iter(op(result, car(rest)), cdr(rest))
        }
    }
    return iter(initial, sequence)
}

func foldr<A,B>(_ op: (A, B) -> B, initial: B, seq sequence: [A]) -> B {
    if sequence.isEmpty {
        return initial
    } else {
        return op(car(sequence), foldr(op, initial:initial, seq:cdr(sequence)))
    }
}


foldr(/, initial:64.0, seq:[2,2,2])
foldl(/, initial:1.0, seq:[1,2,3])


enum Tree<T> {
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
        let leaves = values.map { Tree.Leaf($0) }
        return Tree.Node(leaves)
    }
    static func cons(_ left: T, _ right: Tree<T>) -> Tree<T> {
        let l = Tree.leaf(left)
        return Tree.node(l, right)
    }
    static func cons(_ left: Tree<T>, _ right: T) -> Tree<T> {
        let r = Tree.leaf(right)
        return Tree.node(left, r)
    }
}


let a = foldr(Tree.cons, initial:Tree.Node([]), seq:[1,2,3])
a.stringRepresentation

let b = foldl(Tree.cons, initial:Tree.Node([]), seq:[1,2,3])
b.stringRepresentation


// Give a property that op should satisfy to guarantee that fold-right and fold-left will produce the same values for any sequence

// op needs to obey the commutativity property. This means that the function is the same in one direction as it is in the other.
