import Cocoa

// Exercise 1.24


func isEven(n: Int) -> Bool {
    return (n % 2) == 0
}
func square(x: Int) -> Int {
    return x * x
}
func dividesWithNoRemainder(a: Int, b: Int) -> Bool {
    return  a % b == 0
}

func expMod(base: Int, _ exp: Int, _ m: Int) -> Int {
    switch true {
    case exp == 0:
        return 1
    case isEven(exp):
        return square(expMod(base, exp / 2, m)) % m
    default:
        return (base * expMod(base, exp - 1, m)) % m
    }
}
func fermatTest(n: Int) -> Bool {
    func tryIt(a: Int) -> Bool {
        return expMod(a, n, n) == a
    }
    let randomA:Int = Int(arc4random_uniform(UInt32(n - 1)) + 1)
    return tryIt(randomA)
}
fermatTest(8)

func isPrimeFast(n: Int, _ times: Int) -> Bool {
    switch true {
    case times == 0:
        return true
    case fermatTest(n):
        return isPrimeFast(n, times - 1)
    default:
        return false
    }
}

func timedPrimeTest(n: Int) {
    startPrimeTest(n, NSDate())
}
func startPrimeTest(n: Int, _ startTime: NSDate) {
    if isPrimeFast(n, 10) {
        reportPrime(n, -1 * startTime.timeIntervalSinceNow)
    }
}
func reportPrime(n: Int, _ elapsedTime: Double) {
    print("\n")
    print("\(n)")
    print(" *** ")
    print("\(elapsedTime)")
}

// Using this procedure write a searchForPrimes that checks the primality of consecutive odd integers in a specified range. Use your procedure to find the three smallest primes larger than 1000, larger than 10000, larger than 100000, larger than 1000000.

func searchForPrimes(a: Int, _ b: Int) {
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

searchForPrimes(1000, 1030)
searchForPrimes(10000, 10050)
searchForPrimes(100000, 100050)
searchForPrimes(1000000, 1000050)

// How much longer would you take it to find a prime near 1000000 than it is to find a prime near 1000?
// Log(1000) = 3, Log(1000000) = 6 I would expect it to take twice as long

// Do your timing data bear this out?
// Not really it is about 1.8 times as long and all the calculations are super slow. 1.2 to 1.9 seconds.

// Why the discrepency?
// expmod gets called a lot and is a reasonably complicated procedure???


