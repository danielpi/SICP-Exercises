import Cocoa

// Exercise 2.40
// Define a procedure uniquePairs that, given an integer n, generates the sequence of pairs (i, j) with 1 <= j <= i <= n. Use uniquePairs to simplify the definition of primeSumPairs

func uniquePairs(_ n: Int) -> [(Int,Int)] {
    return Array(1...n).flatMap() { i in
        Array(1..<i).map() { j in
            (i,j)
        }
    }
}

let a = uniquePairs(6)
print("\(a)")

func square(_ x: Int) -> Int {
    return x * x
}

func dividesWithNoRemainder(_ a: Int, _ b: Int) -> Bool {
    return a % b == 0
}

func findDivisor(_ n: Int, _ testDivisor: Int) -> Int {
    switch true {
    case square(testDivisor) > n:
        return n
    case dividesWithNoRemainder(n, testDivisor):
        return testDivisor
    default:
        return findDivisor(n, testDivisor + 1)
    }
}

func smallestDivisor(_ n: Int) -> Int {
    return findDivisor(n, 2)
}

func isPrime(_ n: Int) -> Bool {
    return n == smallestDivisor(n)
}

typealias Pair = (Int,Int)
typealias Triple = (Int,Int,Int)

func isPrimeSum(_ pair: Pair) -> Bool {
    return isPrime(pair.0 + pair.1)
}

func pairToTriple(_ pair: Pair) -> Triple {
    return (pair.0, pair.1, pair.0 + pair.1)
}

func primeSumPairs(_ n: Int) -> [Triple] {
    return uniquePairs(n).filter(isPrimeSum).map(pairToTriple)
}

print("\(primeSumPairs(10))")

isPrime(199)



