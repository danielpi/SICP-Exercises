import Cocoa

// Exercise 1.33
// You can obtain an even more general version of accumulate by introducing the notion of a filter on the terms to be combined. That is, combine only those terms derived from values in the range that satisfy a specified condition. The resulting filteredAccumulate() abstraction takes the same arguments as accumulate, together with an addiional predicate of one argument that specifies the filter.

func filteredAccumulate(_ predicate:(Int) -> Bool, _ combiner: (Double, Double) -> Double, _ nullValue: Double, _ term:(Int) -> Double, _ a: Int, _ next:(Int) -> Int, _ b: Int) -> Double {
    switch true {
    case a > b:
        return nullValue
    case predicate(a):
        return combiner(term(a), filteredAccumulate(predicate, combiner, nullValue, term, next(a), next, b))
    default:
        return filteredAccumulate(predicate, combiner, nullValue, term, next(a), next, b)
    }
}

// a) Write the sum of the squares of the prime numbers in the interval a to b (assuming that you have an isPrime predicate already written.

func isEven(_ n: Int) -> Bool {
    return (n % 2) == 0
}
func square(_ x: Int) -> Int {
    return x * x
}
func dividesWithNoRemainder(_ a: Int, _ b: Int) -> Bool {
    return  a % b == 0
}
func next(_ n: Int) -> Int {
    if n == 2 {
        return 3
    } else {
        return n + 2
    }
}
func findDivisor(_ n: Int, _ testDivisor: Int) -> Int {
    switch true {
    case square(testDivisor) > n:
        return n
    case dividesWithNoRemainder(n, testDivisor):
        return testDivisor
    default:
        return findDivisor(n, next(testDivisor))
    }
}
func inc(_ n: Int) -> Int { return n + 1 }

//func identity(x: Int) -> Int { return x }

func smallestDivisor(_ n: Int) -> Int {
    return findDivisor(n, 2)
}

func isPrime(_ n:Int) -> Bool {
    if n > 1 {
        return n == smallestDivisor(n)
    } else {
        return false
    }
}

func sumOfSquaresOfPrimes(_ a: Int, _ b: Int) -> Double {
    
    func squareDouble(_ n:Int) -> Double { return Double(n * n) }
    return filteredAccumulate(isPrime, +, 0.0, squareDouble, a, inc, b)
}

sumOfSquaresOfPrimes(1, 10)


// b) Write the product of all the positive integers less than n that are relatively prime to n(i.e., all positive integers i < n such that GCD(i,n) = 1)

func gcd(_ a: Int, _ b: Int) -> Int {
    if b == 0 {
        return a
    } else {
        return gcd(b, a % b)
    }
}


func productOfRelativelyPrime(_ n: Int) -> Double {
    func filter(a: Int) -> Bool {
        return gcd(a, n) == 1
    }
    func identity(_ x:Int) -> Double { return Double(x) }
    return filteredAccumulate(filter, *, 1.0, identity, 2, inc, n)
}
productOfRelativelyPrime(10)



