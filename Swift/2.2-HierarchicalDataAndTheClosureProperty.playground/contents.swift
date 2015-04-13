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




