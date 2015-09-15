import Cocoa

// Exercise 2.34
// Evaluating a polynomial in x at a given value of x can be formulated as an accumulation. We evaluate the polynomial

// anx^n + an-1x^n-1 + ... a1x + a0

// using a well-known algorithm called Horner's rule, which structures the computation as 

// (...((an)x + an-1)x + ... + a1)x + a0

// In other words, we start with an, multiply by x, add an-1, multiply by x and so on, until we reach a0.

// Fill in the following template to produce a procedure that evaluates a polynomial using Horner's rule. Assume that the coefficients of the polynomial are arranged in a sequence, from a0 through an

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
func accumulate(op: (Double, Double) -> Double, initial: Double, sequence: [Double]) -> Double {
    if sequence.isEmpty {
        return initial
    } else {
        return op(car(sequence), accumulate(op, initial: initial, sequence: cdr(sequence)))
    }
}

func hornerEval(x: Double, coefficientSequence: [Double]) -> Double {
    return accumulate({ (thisCoeff, higherTerms) in return (higherTerms * x) + thisCoeff }, initial: 0.0, sequence: coefficientSequence)
}

// For example to compute 1 + 3x + 5x^3 + x^5 at x = 2 you would evaluate

hornerEval(2, coefficientSequence: [1,3,0,5,0,1])

func hornerEval2(x: Double, coefficientSequence: [Double]) -> Double {
    return coefficientSequence.reduce(0.0) { ($0 * x) + $1 }
}
hornerEval2(2, coefficientSequence: [1,0,5,0,3,1])
// Note that t