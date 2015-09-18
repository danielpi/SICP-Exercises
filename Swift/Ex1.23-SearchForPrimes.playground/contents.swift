
import Cocoa

// Exercise 1.23
// The smallestDivisor procedure shown at the start of this section does lots of needless testing. After it checks to see if the number is divisible by 2 there is no point in checking to see if it is divisible by any larger even numbers. Define a procedure next that returns 3 if its input is equal to 2 and otherwise returns its input + 2

func square(x: Int) -> Int {
    return x * x
}
func dividesWithNoRemainder(a: Int, _ b: Int) -> Bool {
    return  a % b == 0
}
func isEven(x:Int) -> Bool {
    return dividesWithNoRemainder(x, 2)
}
func findDivisor(n: Int, _ testDivisor: Int) -> Int {
    switch true {
    case square(testDivisor) > n:
        return n
    case dividesWithNoRemainder(n, testDivisor):
        return testDivisor
    default:
        return findDivisor(n, next(testDivisor))
    }
}

func smallestDivisor(n: Int) -> Int {
    return findDivisor(n, 2)
}

func isPrime(n:Int) -> Bool {
    return n == smallestDivisor(n)
}

func timedPrimeTest(n: Int) {
    startPrimeTest(n, NSDate())
}
func startPrimeTest(n: Int, _ startTime: NSDate) {
    if isPrime(n) {
        reportPrime(n, -1 * startTime.timeIntervalSinceNow)
    }
}
func reportPrime(n: Int, _ elapsedTime: Double) {
    print("\n")
    print("\(n)")
    print(" *** ")
    print("\(elapsedTime)")
}

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

func next(n: Int) -> Int {
    if n == 2 {
        return 3
    } else {
        return n + 2
    }
}

searchForPrimes(1000, 1030)
//searchForPrimes(10000, 10050)
//searchForPrimes(100000, 100050)
//searchForPrimes(1000000, 1000050)


/*
Exercise 1.22 ran in the following time
1009 *** 0.189490020275116
1013 *** 0.199604034423828
1019 *** 0.215083003044128
1021 *** 0.211998045444489

Exercise 1.23 ran in the following time
1009 *** 0.187184989452362
1013 *** 0.294092953205109
1019 *** 0.20736700296402
1021 *** 0.197423994541168


Exercise 1.22 ran in the following time
1000003 *** 7.16602700948715
1000033 *** 8.14103001356125
1000037 *** 7.98269999027252
1000039 *** 7.72472196817398

Exercise 1.23 ran in the following time
1000003 *** 6.5731880068779
1000033 *** 5.61217302083969
1000037 *** 5.32903099060059
1000039 *** 5.41905397176743

Looks like they both ran in about the same amount of time for the lower primes. Its 2015-02-02 and Swift playground performance is a bit screwy
For the higher primes we get a ration of about 1.4 rather than 2. Possibly this is due to the if statement in next() (and screwy Swift performance)
*/
 7.72472196817398 / 5.41905397176743
