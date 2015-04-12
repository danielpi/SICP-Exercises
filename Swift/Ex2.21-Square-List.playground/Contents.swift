import Cocoa

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
func map<T, U>(proc:(T) -> U, items: [T]) -> [U] {
    if items.isEmpty {
        return []
    } else {
        return cons(proc(car(items)), map(proc, cdr(items)))
    }
}


// Exercise 2.21
// The procedure square-list takes a list of numbers as argument and returns a lis of the squares of those numbers. Here are two different deffinitions of square-list. Complete both of them by filling in the missing expressions:

func square(x: Int) -> Int {
    return x * x
}

func squareList(items: [Int]) -> [Int] {
    if items.isEmpty {
        return []
    } else {
        return cons(square(car(items)), squareList(cdr(items)))
    }
}
squareList([1, 2, 3, 4])


func squareList2(items: [Int]) -> [Int] {
    return map(square, items)
}
squareList2([1, 2, 3, 4])

