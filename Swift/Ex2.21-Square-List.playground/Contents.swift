import Cocoa

// Exercise 2.21
// The procedure square-list takes a list of numbers as argument and returns a lis of the squares of those numbers. Here are two different deffinitions of square-list. Complete both of them by filling in the missing expressions:

func square(x: Int) -> Int {
    return x * x
}

func squareList(items: List<Int>) -> List<Int> {
    if items.isEmpty() {
        return []
    } else {
        return cons(square(car(items)!), right: squareList(cdr(items)!))
    }
}
print("\(squareList([1, 2, 3, 4]))")


func squareList2(items: List<Int>) -> List<Int> {
    return items.map(square)
}
print("\(squareList2([1, 2, 3, 4]))")