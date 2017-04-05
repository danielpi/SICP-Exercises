
import Cocoa

// Exercise 1.23
// The smallestDivisor procedure shown at the start of this section does lots of needless testing. After it checks to see if the number is divisible by 2 there is no point in checking to see if it is divisible by any larger even numbers. Define a procedure next that returns 3 if its input is equal to 2 and otherwise returns its input + 2

func square(_ x: Int) -> Int {
    return x * x
}
func dividesWithNoRemainder(_ a: Int, _ b: Int) -> Bool {
    return  a % b == 0
}
func isEven(_ x:Int) -> Bool {
    return dividesWithNoRemainder(x, 2)
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

func smallestDivisor(_ n: Int) -> Int {
    return findDivisor(n, 2)
}

func isPrime(_ n:Int) -> Bool {
    return n == smallestDivisor(n)
}

func timedPrimeTest(_ n: Int) {
    startPrimeTest(n, NSDate())
}
func startPrimeTest(_ n: Int, _ startTime: NSDate) {
    if isPrime(n) {
        reportPrime(n, -1 * startTime.timeIntervalSinceNow)
    }
}
func reportPrime(_ n: Int, _ elapsedTime: Double) {
    print("\(n) *** \(elapsedTime)")
}

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

func next(_ n: Int) -> Int {
    if n == 2 {
        return 3
    } else {
        return n + 2
    }
}

searchForPrimes(1000, 1030)
//searchForPrimes(10000, 10050)
//searchForPrimes(100000, 100050)
searchForPrimes(1000000, 1000050)


/*
Exercise 1.22 ran in the following time
 1009 *** 0.00457704067230225
 1013 *** 0.00441199541091919
 1019 *** 0.00452703237533569

Exercise 1.23 ran in the following time
 1009 *** 0.00326997041702271
 1013 *** 0.00355499982833862
 1019 *** 0.00319898128509521
 1021 *** 0.00329303741455078


Exercise 1.22 ran in the following time
 1000003 *** 0.157728970050812
 1000033 *** 0.194114983081818
 1000037 *** 0.196036040782928

Exercise 1.23 ran in the following time
 1000003 *** 0.142865002155304
 1000033 *** 0.122924983501434
 1000037 *** 0.1805340051651
 1000039 *** 0.170925974845886

Looks like they both ran in about the same amount of time for the lower primes. Its 2015-02-02 and Swift playground performance is a bit screwy
For the higher primes we get a ration of about 1.4 rather than 2. Possibly this is due to the if statement in next() (and screwy Swift performance)
*/
 7.72472196817398 / 5.41905397176743
