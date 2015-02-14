import Cocoa

// 1.3 Formulating Abstractions with Higher-Order Procedures

// Procedures are abstractions that describe compound operations on numbers independent of the particular numbers. For instance.

func cube(x: Int) -> Int {
    return x * x * x
}

// We are not talking about the cube of a particular number, but rather a method of obtaining the cube of any number. This is not strictly nessesary however a language without this ability would be at a serious disadvantage. You would be limited to the set of primitive operations that were a part of the language.
// Yet even in numerical processing we will be severely limited in our ability to create abstractions if we are restricted to procedures whose parameters must be numbers. Higher-order procedures are procedures that can accept or return other procedures.


// 1.3.1 Procedures as Arguments
// Consider the following three procedures

func sumIntegers(a: Int, b: Int) -> Int {
    if a > b {
        return 0
    } else {
        return a + sumIntegers(a + 1, b)
    }
}
sumIntegers(1, 10)

func sumCubes(a: Int, b: Int) -> Int {
    if a > b {
        return 0
    } else {
        return cube(a) + sumCubes(a + 1, b)
    }
}
sumCubes(1, 10)

func piSum(a: Int, b: Int) -> Float {
    if a > b {
        return 0
    } else {
        return (1.0 / (Float(a) * (Float(a) + 2.0))) + piSum(a + 4, b)
    }
}
piSum(1, 100) * 8

// These three procedures clearly share a common underlying pattern. We could generate each of the procedures by filling in slots in the same template
/*
func <name>(a: Int, b: Int) -> ??? {
    if a > b {
        return 0
    } else {
        return <term>(a) + <name>(<next>(a), b)
    }
}
*/


