import Cocoa

// Exercise 2.41
// Write a procedure to find all ordered triples of distinct positive integers i, j and k less than or equal to a given integer n that sum ot a given integer s.

typealias Triple = (Int,Int,Int)

// I need to be able to generate all possible triple combinations
func tripleCombinations(n: Int) -> [Triple] {
    return Array(1...n).flatMap() {
        i in Array(1..<i).flatMap() {
            j in Array(1..<j).map() {
                k in Triple(i,j,k)
            }
        }
    }
}
println("\(tripleCombinations(4))")

// Now I need a function that can tell me if the sum of a triple is equal to s
func tripleSum(triple: Triple) -> Int {
    return triple.0 + triple.1 + triple.2
}
func isTripleSumEqualTo(target: Int) -> (Triple) -> Bool {
    return { triple in
        return tripleSum(triple) == target
    }
}

func tripleSumCombinations(n: Int, s: Int) -> [Triple] {
    //return tripleCombinations(n).filter() { tripleSum($0) == s } // I think this is clearer and requires less code.
    return tripleCombinations(n).filter(isTripleSumEqualTo(s)) // This make further chaining easier
}

println("\(tripleSumCombinations(10, 14))")



