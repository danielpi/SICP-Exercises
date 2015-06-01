import Cocoa

// 2.2 Hierarchical Data and the Closure Property
// As we have seen, pairs provide a primitive "glue" that we can use to contruct compound data objects. We have already seen that cons can be used to combine not only numbers but pairs as well. As a consequence, pairs provide a universal building block from which we can construct all sorts of data structures.

// The ability to create pairs whose elements are pairs is the essence of list structure's importane as a representational tool. We refer to this ability as the closure property of cons. In general, an operation for combining data objects satisfies the closure property if the results of combining things with that operation can themselves be combined using the same operation. Closure is the key to power in any means of combination because it permits us to create hierarchical structures -- structures made up of parts, which themselves are made up of parts, and so on.



struct Pair<A,B> {
    let left: A
    let right: B
}

func cons<A,B>(left: A, right: B) -> Pair<A,B> {
    return Pair(left: left, right: right)
}
func car<A,B>(pair: Pair<A,B>) -> A {
    return pair.left
}
func cdr<A,B>(pair: Pair<A,B>) -> B {
    return pair.right
}

// One of the useful structures we can build with pairs is a sequence -- an ordered collection of data objects. There are many ways to represent sequences in terms of pairs. One particularly straightforward representation is illustrated below for the sequence 1 2 3 4.

let abcd = cons(1, cons(2, cons(3, cons(4,()))))

/*
let ab = cons(1, cons(2, ()))
let cd = cons(3, cons(4, ()))
let acdb = cons(ab, cd)

println("\(acdb)")

extension Array {
    var decompose : (head: T, tail: [T])? {
        return (count > 0) ? (self[0], Array(self[1..<count])) : nil
    }
}

func list<T,U>(values: [T]) -> U {
    if let (head, tail) = values.decompose {
        return cons(head, list(tail))
    }
}
*/
cons(1, cons(2, cons(3, cons(4, []))))

// Lisp systems conventionally print lists by printing the sequence of elements, enclosed in parentheses. In Swift a list literal looks like this [1, 2, 3, 4] but that isn't implemented via cons. I've had trouble writing a list function in Swift (I couldn't seem to make the generics work) so I'm switching to using an array as the base of the my lists. Thus I need car and cdr re-written to accept arrays.
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

// We can think of car as selecting the first item in the list, and of cdr as selecting the sublist consisting of all but the first item. Nested applications of car and cdr can be used to extract the second, third and subsequent items in the list.

let oneThroughFour = [1,2,3,4]
car(oneThroughFour)
cdr(oneThroughFour)

// cons(10, oneThroughFour)
// cons(5, oneThroughFour)


// List operations
// The use of pairs to represent sequences of elements as lists is accompanied by conventional programming techniques for manipulating lists by successively "cdring down" the lists. For example, the procedure list-ref takes as arguments a list and a number n and returns the nth item of the list. It is customary to number the elements of the list beginning with 0. The method for computing list-ref is the following:
// - For n == 0, list-ref should return the car of the list.
// - Otherwise, list-ref should return the (n-1)st item of the cdr of the list

func listRef<A>(items:[A], n: Int) -> A {
    if n == 0 {
        return car(items)
    } else {
        return listRef(cdr(items), n - 1)
    }
}
let squares = [1, 4, 9, 16, 25]
listRef(squares, 3)

// Often we cdr down the whole list. To aid in this, Swift includes a primitive predicate isEmpty, which tests whether its argument is the empty list. The procedure length, which returns the number of items in a list, illustrates this typical pattern of use:

func length<A>(items: [A]) -> Int {
    if items.isEmpty {
        return 0
    } else {
        return length(cdr(items)) + 1
    }
}
let odds = [1, 3, 5, 7]
length(odds)

// The length procedure implements a simple recursive plan. The reduction step is
// - The length of any list is 1 plus the length of the cdr of the list.
// This is applied successively until we reach the base case:
// - The length of the empty list is 0

// We could also compute length in an iterative style

func length2<A>(items: [A]) -> Int {
    var lengthIter: ([A], Int) -> Int = { _, _ in return 0 }
    lengthIter = { a, count in
        if a.isEmpty {
            return count
        } else {
            return lengthIter(cdr(a), count + 1)
        }
    }
    return lengthIter(items, 0)
}
length2(odds)

// Another conventional programming technique is to "cons up" an answer list while cdring down a list, as in the procedure append, which takes two lists as arguments and combines their elements to make a new list:

// Append is also implemented using a recursive plan. To append lists list1 and list2, do the following
// - If list1 is the empty list, then the result is just list2
// - Otherwise, append the cdr of list1 and list2, and cons the car of list1 onto the result

func append<A>(list1: [A], list2: [A]) -> [A] {
    if list1.isEmpty {
        return list2
    } else {
        return cons(car(list1), append(cdr(list1), list2))
    }
}

append(squares, odds)
append(odds, squares)


// Mapping over lists
// One extremely useful operation is to apply some transformation to each element in a list and generate the list of results. For instance, the following procedure scales each number in a list by a given factor:

func scaleList(items: [Double], factor: Double) -> [Double] {
    if items.isEmpty {
        return []
    } else {
        return cons(car(items) * factor, scaleList(cdr(items), factor))
    }
}
scaleList([1, 2, 3, 4, 5], 10)

// We can abstract this general idea and capture it as a common pattern expressed as a higher-order procedure, just as in section 1.3. The higher-order procedure here is called map. Map takes as arguments a procedure of one argument and a list, and returns a list of the results produced by applying the procedure to each element in the list:

func map<T, U>(proc:(T) -> U, items: [T]) -> [U] {
    if items.isEmpty {
        return []
    } else {
        return cons(proc(car(items)), map(proc, cdr(items)))
    }
}

map(abs, [-10, 2.5, -11.6, 17])
map({ x in x * x }, [1, 2, 3, 4])
// Now we can give a new definition of scaleList in terms of map:
func scaleList2(items: [Double], factor: Double) -> [Double] {
    return map({ x in x * factor }, items)
}
scaleList2([1, 2, 3, 4, 5], 10)

// Map is an important construct not only because it captures a common pattern, but because it establishes a higher level of abstraction in dealing with lists. In the original definition of scaleList, the recursive structure of the program draws attention to the element-by-element processing of the list. Defining scaleList in terms of map suppresses that level of detail and emphasizes that scaling transforms a list of elements to a list of results. The difference between the two definitions is not that the computer is performing a different process (it isn't) but that we think about the process differently. In effect, map helps establish an abstraction barrier that isolates the implementation of procedures that trahsform lists from the details of how the elements of the list are extracted and combined. Like the barriers shown in figure 2.1, this abstraction gives us the flexibility to change the low-level details of how sequences are implemented, while preserving the conceptual framework of operations that transform sequences to sequences.


// 2.2.2 Hierarchical Structures
// The representation of sequences in terms of lists generalizes naturally to represent sequences whose elements may themselves be sequences. For example, we can regard the object ((1 2) 3 4) constructed by

cons([1, 2], [3, 4])

// as a list of three items, the first of which is itself a list, (1 2). Indeed, this is suggested by the form in which the result is printed by the interpreter.

// Another way to think of sequences whose elements are sequences is as trees. The elements of the sequence are the branches of the tree, and elements that are themselves sequences are subtrees.

// Recursion is a natural tool for dealing with tree structures, since we can often reduce operations on trees to operations on their branches, which reduce in turn to operations on the branches of the branches, and so on, until we reach the leaves of the tree. As an example, compare the length procedure of section 2.2.1 with the count-leaves procedure, which returns the total number of leaves of a tree.

// To implement count-leaves, recall the recursive plan for computing length:
// - Length of a list x is 1 plus length of the cdr of x
// - Length of the empty list is 0

// Count-leaves is similar. The value for the empty list is the same:
// - Count-leaves of the empty list is 0
// But in the reduction step, where we strip off the car of the list, we must take into account that the car may itself be a tree whose leaves we need to count. Thus, the appropriate reduction step is
// - Count-leaves of a tree x is count-leaves of the car of x plus count-leaves of the cdr of x

// Finally, by taking cars we reach actual leaves, so we need another base case:
// - Count-leaves of a leaf is 1.

// To aid in writing recursive procedures on trees, Scheme provides the primitive predicate pair?, which tests whether its argument is a pair. Here is the complete procedure

func countLeaves(x: Int) -> Int {
    return 1
}

func countLeaves(x: [Int]) -> Int {
    switch true {
    case x.isEmpty:
        return 0
    default:
        return countLeaves(car(x)) + countLeaves(cdr(x))
    }
}

func countLeaves(x: Pair<[Int],[Int]>) -> Int {
    return countLeaves(car(x)) + countLeaves(cdr(x))
}

let x = cons([1,2],[3,4])
countLeaves(x)


// Mapping over trees
// Just as map is a powerful abstraction for dealing with sequences, map together with recursion is a powerful abstraction for dealing with trees. For instance, the scale-tree procedure, analogous to scale-list of section 2.2.1 takes as arguments a numeric factor and a tree whose leaves are numbers. It returns a tree of the same shape, where each number is multiplied by the factor. The recursive plan for scale-tree is similar to the one for count-leaves:

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

//let xx = Tree.Node(Box([Tree.Node(Box([Tree.Leaf(Box(1)),Tree.Leaf(Box(2))]),Tree.Leaf(Box(3)), Tree.Leaf(Box(4)))]))

let a = Tree.Node([Box(.Leaf(Box(3))), Box(.Leaf(Box(4)))])
a.stringRepresentation


let b = Tree.node(Tree.leaf(1), Tree.node(Tree.leaf(2), Tree.list(3,4), Tree.leaf(5), Tree.list(6,7)))
println("\(b.stringRepresentation)")

protocol Multipliable: Equatable {
    func *(lhs: Self, rhs: Self) -> Self
}
extension Int: Multipliable {}
extension Double: Multipliable {}

func scaleTree<T: Multipliable>(tree: Tree<T>, factor: T) -> Tree<T> {
    switch tree {
    case let .Leaf(value):
        return Tree.leaf(value.unbox * factor)
    case let .Node(values):
        let newValues = map(values) { Box(scaleTree($0.unbox, factor)) }
        return Tree.Node(newValues)
    }
}

let c = scaleTree(b, 10)
c.stringRepresentation


// 2.2.3 Sequences as Conventional Interfaces

// In working with compound data, we've stressed how data abstraction permits us to design programs without becoming enmeshed in the details of data representations, and how abstraction preserves for us the flexibility to experiment with alternative representations. In this section we introduce another powerful design principle for working with data structures - the use of conventional interfaces.
// In Section 1.3 we say how program abstractions, implemented as higher-order procedures, can capture common patterns in programs that deal with numerical data. Our ability to formulate analogous operations for working with compound data depends crucially on the style in which we manipulate our data structures. Consider, for example, the following procedure, analogous to the count-leaves procedure of section 2.2.2, which takes a tree as argument and computes the sum of the squares of the leaves that are odd:

func isEven(x: Int) -> Bool {
    return x % 2 == 0
}
func isOdd(x: Int) -> Bool {
    return x % 2 != 0
}

isOdd(1)
isOdd(2)
isOdd(3)

func square(x: Int) -> Int { return x * x }

func sumOddSquares(tree: Tree<Int>) -> Int {
    switch tree {
    case .Leaf(let value):
        return isOdd(value.unbox) ? square(value.unbox) : 0
    case .Node(let values):
        return reduce(values, 0) { $0 + sumOddSquares($1.unbox) }
    }
}

1 + (3*3) + (5*5) + (7*7)
sumOddSquares(b)

//On the surface, this procedure is very different from the following one, which constructs a list of all the even Fibonacci numbers Fib(k), where k is less than or equal to a given integer n:

func fib(n: Int) -> Int {
    switch true {
    case n == 0:
        return 0
    case n == 1:
        return 1
    default:
        return fib(n - 1) + fib(n - 2)
    }
}

func evenFibs(fromFirstNFibs n: Int) -> [Int] {
    var next: (Int) -> [Int] = { _ in return [] }
    next = { (k: Int) in
        if k > n {
            return []
        } else {
            let f = fib(k)
            if isEven(f) {
                return cons(f, next(k + 1))
            } else {
                return next(k + 1)
            }
        }
    }
    return next(0)
}

evenFibs(fromFirstNFibs: 10)

// Despite the fact that these two procedures are structurally very different, a more abstract description of the two computations reveals a great deal of similarity. The first program
// - Enumerates the leaves of a tree
// - filters them, selecting the odd ones
// - squares each of the selected ones, and
// - accumulates the results using +, starting with 0

// The second program
// - Enumerates the integers from 0 to n
// - computes the Fibonacci number for each integer
// - filters them, selecting the even ones, and
// - accumulates the results using cons, starting with an empty list

// A signal processing engineer would find it natural to conceptualize these processes in terms of signals flowing through a cascade of stages, each of which implements part of the program plan. In sum-odd-squares, we begin with an enumerator, which generates a signal consisting of the leaves of a given tree. This signal is passed through a filter, which eliminates all but the odd elements. The resulting signal is in turn passed through a map, which is a transducer that applies the square procedure to each element. The output of the map is then fed to an accumulator, which combines the elements using +, starting from an initial 0. The plan for even-fibs is analogous.
// Unfortunately, the two procedure definitions above fail to exhibit this signal-flow structure. For instance, if we examine the sum-odd-squares procedure, we find that the enumeration is implemented partly by the null? and pair? tests and partly bu the tree-recursive structure of the procedure. Similarly, the accumulation is found partly in the tests and partly in the addition used in the recursion In general, there are no distinct parts of either procedure that correspond to the elements in the signal-flow description. Our two procedures decompose the computations in a different way, spreading the enumeration over the program mingling it with the map, the filter and the accumulation. If we could organize our programs to make the signal-flow structure manifest in the procedures we write, this would increase the conceptual clarity of the resulting code.


// Sequence Operations
// The key to organising programs so as to more clearly reflect the signal-flow structure is to concentrate on the "signals" that flow from one stage in the process to the next. If we represent these signals as lists, then we can use list operations to implement the processing at each of the stages. For instance, we can implement the mapping stages of the signal-flow diagrams using the map procedure from section 2.2.1

map([1,2,3,4,5], square)

//Filtering a sequence to select only those elements that satisfy a given predicate is accomplished by

func filterDRP(predicate: (Int) -> Bool, sequence: [Int]) -> [Int] {
    switch true {
    case sequence.isEmpty:
        return []
    case predicate(car(sequence)):
        return cons(car(sequence), filterDRP(predicate, cdr(sequence)))
    default:
        return filterDRP(predicate, cdr(sequence))
    }
}

filterDRP(isOdd, [1,2,3,4,5])
filter([1,2,3,4,5], isOdd)

// Accumulations can be implemented by
func accumulate(op: (Int, Int) -> Int, initial: Int, sequence: [Int]) -> Int {
    if sequence.isEmpty {
        return initial
    } else {
        return op(car(sequence), accumulate(op, initial, cdr(sequence)))
    }
}

accumulate(+, 0, [1,2,3,4,5])
accumulate(*, 1, [1,2,3,4,5])
// accumulate(cons, [], [1,2,3,4,5]) // Needs a generic version of accumulate.

reduce([1,2,3,4,5], 0, +)
reduce([1,2,3,4,5], 1, *)

// All that remains to implement signal-flow diagrams is to enumerate the sequence of elements to be processed. For even-fibs, we need to generate the sequence of integers in a given range, which we can do as follows

func enumerateInterval(low: Int, high:Int) -> [Int] {
    if low > high {
        return []
    } else  {
        return cons(low, enumerateInterval(low + 1, high))
    }
}

enumerateInterval(3, 10)

// or in Swift
Array(3...10)

// TO enumerate the leaves of a tree we can use
func enumerateTree(tree: Tree<Int>) -> [Int] {
    switch tree {
    case .Leaf(let value):
        return [value.unbox]
    case .Node(let values):
        return reduce(values, []) { $0 + enumerateTree($1.unbox) }
    }
}

enumerateTree(b)
enumerateTree(c)

// Now we can reformulate sum-odd-squares and even-fibs as in the signal-flow diagrams. For sum-odd-squares, we enumerate the sequence of leaves of the tree, filter this to keep only the odd numbers in the sequence, square each element, and sum the results

func sumOddSquares2(tree: Tree<Int>) -> Int {
    return accumulate(+, 0, map(filterDRP(isOdd, enumerateTree(b)), square))
}
sumOddSquares2(b)

func sumOddSquares3(tree: Tree<Int>) -> Int {
    return reduce(map(filter(enumerateTree(b), isOdd), square), 0, +)
}
sumOddSquares3(b)

// For even-fibs, we enumerate the integers from 0 to n, generate the fibonacci number for each of these integers, filter the resulting sequence to keep only the even elements, and accumulate the results into a list

func evenFibs2(n: Int) -> [Int] {
    return reduce(filter(map(0...n, fib), isEven), []) { $0 + [$1] }
}
evenFibs2(10)

// The value of expressing programs as sequence operations is that this helps us make program designs that are modular, that is, designs that are constructed by combining relatively independent pieces. We can encourage modular design by providing a library of standard componsnts together with a conventional intergace for connecting the components in flexible ways.
// Modular construction is a powerful strategy for controlling complexity in engineering design. In real signal-processing applications, for example, designers regularly build systems by cascading elements selected from standardized families of filters and transducers. Similarly, sequence operations provide a library of standard program elements that we can mix and match. For instance, we can reuse pieces from the sum-odd-squares and even-fibs procedures in a program that constructs a list of the squares of the first n + 1 Fibonaccy numbers:

func listFibSquares(n: Int) -> [Int] {
    return map(map(0...n, fib), square)
}
listFibSquares(10)

// We can rearrange the pieces and use them in computing the product of the squares of the odd integers in a sequence:
func productOfSquaresOfOddElements(sequence: [Int]) -> Int {
    return reduce(map(filter(sequence, isOdd), square), 1, *)
}
productOfSquaresOfOddElements(Array(1...5))

// We can also formulate conventional data-processing applications in terms of sequence operations. Suppose we have a sequence of personnel records and we want to find the salary of the highest-paid programmer. Assume that we have a selector salary that returns the salary of a record, and a predicate programmer? that tests if a record is for a programmer. Then we can write
struct Employee {
    let job: String
    let salary: Int
}

func salaryOfHighestPaidProgrammer(records: [Employee]) -> Int {
    return reduce(filter(records) { $0.job == "Programmer" }, 0) { max($0, $1.salary) }
}

let employees = [Employee(job: "Programmer", salary: 90000),Employee(job: "Programmer", salary: 70000), Employee(job: "Programmer", salary: 90010), Employee(job: "Gamer", salary: 900000)]
salaryOfHighestPaidProgrammer(employees)

// These examples give just a hint of the vast range of operations that can be expressed as sequence operations.
// Sequences, implemented here as lists, serve as a conventional intergace that permits us to combine processing modules. Additionally, when we uniformly represent structures as sequences, we have localized the data-structure dependencies in our programs to a small number of sequence operations. By changing these we can experiment with alternative representations of sequences, while leaving the overall design of our programs intact. We will exploit this capability in Section 3.5, when we generalize the sequence-processing paradigm to admit infinite sequences.






