import Cocoa

// Exercise 1.25

// What if we change the expMod function to the following.

func expMod(_ base: Int, _ exp: Int, _ m: Int) -> Int {
    return fastExpt(base, exp) % m
}

func fastExpt(_ b: Int, _ n: Int) -> Int {
    switch true {
    case n == 0:
        return 1
    case isEven(n):
        return square(fastExpt(b, n / 2))
    default:
        return b * fastExpt(b, n - 1)
    }
}

func isEven(_ n: Int) -> Bool {
    return (n % 2) == 0
}
func square(_ x: Int) -> Int {
    return x * x
}
func dividesWithNoRemainder(_ a: Int, _ b: Int) -> Bool {
    return  a % b == 0
}
/*
func expMod(base: Int, exp: Int, m: Int) -> Int {
    switch true {
    case exp == 0:
        return 1
    case isEven(exp):
        return square(expMod(base, exp / 2, m)) % m
    default:
        return (base * expMod(base, exp - 1, m)) % m
    }
}
*/
func fermatTest(_ n: Int) -> Bool {
    func tryIt(_ a: Int) -> Bool {
        return expMod(a, n, n) == a
    }
    let randomA:Int = Int(arc4random_uniform(UInt32(n - 1)) + 1)
    return tryIt(randomA)
}
fermatTest(8)

func isPrimeFast(_ n: Int, _ times: Int) -> Bool {
    switch true {
    case times == 0:
        return true
    case fermatTest(n):
        return isPrimeFast(n, times - 1)
    default:
        return false
    }
}

func timedPrimeTest(_ n: Int) {
    startPrimeTest(n, NSDate())
}
func startPrimeTest(_ n: Int, _ startTime: NSDate) {
    if isPrimeFast(n, 10) {
        reportPrime(n, -1 * startTime.timeIntervalSinceNow)
    }
}
func reportPrime(_ n: Int, _ elapsedTime: Double) {
    print("\(n) *** \(elapsedTime)")
}

// Using this procedure write a searchForPrimes that checks the primality of consecutive odd integers in a specified range. Use your procedure to find the three smallest primes larger than 1000, larger than 10000, larger than 100000, larger than 1000000.

func searchForPrimes(_ a: Int, _ b: Int) {
    switch true {
    case a > b:
        print("Complete")
    case isEven(a):
        searchForPrimes(a + 1, b)
    default:
        timedPrimeTest(a)
        searchForPrimes(a + 2, b)
    }
}

// Use your procedure to find the three smallest primes larger than
// - 1000
// - 10000
// - 100000
// - 1000000

searchForPrimes(10, 15)
//searchForPrimes(10000, 10050)
//searchForPrimes(100000, 100050)
//searchForPrimes(1000000, 1000050)

// The fastExpt function creates very large intermediate values. As such it is not useful for calculating primes that are larger than about 15.


