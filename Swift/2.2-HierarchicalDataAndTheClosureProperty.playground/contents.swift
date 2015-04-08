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






