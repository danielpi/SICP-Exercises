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


// Exercise 2.20
// The procedures +, *, and list take arbitrary numbers of arguments. One way to define such procedures is to use define with dotted-tail notation. In a function definition, a parameter list that has three dots (...) after the last parameter indicates that the final parameter's value will be a list of any remaining arguments

// func f(x: Int, y: Int, z: Int...) -> Int {}

// Use this notation to write a procedure same-parity that takes one or more integers and returns a list of all the arguments that have the same even-odd parity as the first argument.

func isEven(x: Int) -> Bool {
    return (x % 2) == 0
}
func isOdd(x: Int) -> Bool {
    return !isEven(x)
}

func isMatchedParity(x: Int, y: Int) -> Bool {
    if isEven(x) {
        return isEven(y)
    } else {
        return isOdd(y)
    }
}

isMatchedParity(1, 5)
isMatchedParity(1, 4)
isMatchedParity(2, 10)
isMatchedParity(2, 9)

func reverse<A>(list: [A]) -> [A] {
    var reverseIter: ([A], [A]) -> [A] = { _, _ in return [] }
    reverseIter = { input, output in
        if input.isEmpty {
            return output
        } else {
            return reverseIter(cdr(input), cons(car(input), output))
        }
    }
    return reverseIter(list, [])
}

func sameParity(list: Int...) -> [Int] {
    var iter: ([Int], [Int], Int) -> [Int] = { _, _, _ in return [] }
    iter = { input, output, firstValue in
        if input.isEmpty {
            return output
        } else {
            var newOutput = output
            if isMatchedParity(car(input), firstValue) {
                newOutput = cons(car(input), newOutput)
            }
            return iter(cdr(input), newOutput, firstValue)
        }
    }
    return reverse(iter(list, [], car(list)))
}

sameParity(1, 2, 3, 4, 5, 6, 7)
sameParity(2, 3, 4, 5, 6, 7)





