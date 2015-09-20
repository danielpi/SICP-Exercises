import Cocoa

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

func isMatchedParity(x: Int, _ y: Int) -> Bool {
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

func reverse<A>(list: List<A>) -> List<A> {
    var reverseIter: (List<A>, List<A>) -> List<A> = { _, _ in return [] }
    reverseIter = { input, output in
        if input.isEmpty() {
            return output
        } else {
            return reverseIter(cdr(input)!, cons(car(input)!, output))
        }
    }
    return reverseIter(list, [])
}

func sameParity(list: Int...) -> List<Int> {
    var iter: (List<Int>, List<Int>, Int) -> List<Int> = { _, _, _ in return [] }
    iter = { input, output, firstValue in
        if input.isEmpty() {
            return output
        } else {
            var newOutput = output
            if isMatchedParity(car(input)!, firstValue) {
                newOutput = cons(car(input)!, newOutput)
            }
            return iter(cdr(input)!, newOutput, firstValue)
        }
    }
    //var theList: List<Int> = List()
    //for value in list {
    //    theList = cons(value, theList)
    //}
    return reverse(iter(List(list), [], car(List(list))!))
}

sameParity(1, 2, 3, 4, 5, 6, 7)
sameParity(2, 3, 4, 5, 6, 7)

