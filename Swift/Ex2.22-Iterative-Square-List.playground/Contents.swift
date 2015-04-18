import Cocoa

func square(x: Int) -> Int { return x * x }

// Exercise 2.22
// Louis Reasoner tries to rewrite the first square-list procedure of exercise 2.21 so that it evolves an iterative process:


func squareList(items: List<Int>) -> List<Int> {
    var iter: (List<Int>, List<Int>) -> List<Int> = { _, _ in return [] }
    iter = { things, answer in
        if things.isEmpty() {
            return answer
        } else {
            return iter(cdr(things)!, cons(square(car(things)!), answer))
        }
    }
    return iter(items, [])
}
squareList([1, 2, 3, 4])

// Unfortunately, defining squareList this way produces the answer list in the reverse order of the one desired. Why?

// The algorithm works through the list forwards once in order to build up the computation steps which then operate on the list in reverse. This gives us a reversed list in the end.

// Louis then tries to fix his bug by interchanging the arguments to cons:

func squareList2(items: List<Int>) -> List<Int> {
    var iter: (List<Int>, List<Int>) -> List<Int> = { _, _ in return [] }
    iter = { things, answer in
        if things.isEmpty() {
            return answer
        } else {
            return iter(cdr(things)!, cons(answer, square(car(things)!)))
        }
    }
    return iter(items, [])
}


// This doesn't work either. Explain.

// This doesn't work in our case because cons is defined as taking a value and an array of values, not an array of values and a value.
