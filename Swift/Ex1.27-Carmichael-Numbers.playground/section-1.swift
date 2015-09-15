import Darwin

// Exercise 1.27
// Demonstrate that Carmichael numbers really do fool the Fermat test. Write a procedure that takes an integer n and tests whether a^n is congruent to a modulo n for every a < n.


func isEven(n: Int) -> Bool {
    return (n % 2) == 0
}
func square(x: Int) -> Int {
    return x * x
}
func dividesWithNoRemainder(a: Int, b: Int) -> Bool {
    return  a % b == 0
}

func next(n: Int) -> Int {
    if n == 2 {
        return 3
    } else {
        return n + 2
    }
}

func findDivisor(n: Int, testDivisor: Int) -> Int {
    switch true {
    case square(testDivisor) > n:
        return n
    case dividesWithNoRemainder(n, b: testDivisor):
        return testDivisor
    default:
        return findDivisor(n, testDivisor: next(testDivisor))
    }
}

func smallestDivisor(n: Int) -> Int {
    return findDivisor(n, testDivisor: 2)
}

func isPrime(n:Int) -> Bool {
    return n == smallestDivisor(n)
}


func expMod(base: Int, exp: Int, m: Int) -> Int {
    switch true {
    case exp == 0:
        return 1
    case isEven(exp):
        return square(expMod(base, exp: exp / 2, m: m)) % m
    default:
        return (base * expMod(base, exp: exp - 1, m: m)) % m
    }
}

func carmichaelTestIter(n: Int, a: Int) -> Bool {
    switch true {
    case a == 0:
        return true
    case expMod(a, exp: n, m: n) == a:
        return carmichaelTestIter(n, a: a - 1)
    default:
        return false
    }
}

func isCarmichaelNumber(n: Int) -> Bool {
    if isPrime(n) {
        return false
    } else {
        return carmichaelTestIter(n, a: n - 1)
    }
}

//isCarmichaelNumber(7)
isCarmichaelNumber(561)


