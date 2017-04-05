import Cocoa

// Exercise 1.22
func isEven(_ n: Int) -> Bool {
    return (n % 2) == 0
}
func square(_ x: Int) -> Int {
    return x * x
}
func dividesWithNoRemainder(_ a: Int, _ b: Int) -> Bool {
    return  a % b == 0
}
// dividesWithNoRemainder(10, 2)

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
    print("\n")
    print("\(n)")
    print(" *** ")
    print("\(elapsedTime)")
}
// timedPrimeTest(7013)

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

searchForPrimes(1000, 1030)
// 1009 *** 0.00457704067230225
// 1013 *** 0.00441199541091919
// 1019 *** 0.00452703237533569
(0.00457704067230225 + 0.00441199541091919 + 0.00452703237533569) / 3
// Average time 0.004505356152852377

searchForPrimes(10000, 10050)
// 10007 *** 0.0136359930038452
// 10009 *** 0.0135749578475952
// 10037 *** 0.0148789882659912
(0.0136359930038452 + 0.0135749578475952 + 0.0148789882659912) / 3
// Average time 0.01402997970581053

searchForPrimes(100000, 100050)
// 100003 *** 0.0468270182609558
// 100019 *** 0.0487009882926941
// 100043 *** 0.0503749847412109
(0.0468270182609558 + 0.0487009882926941 + 0.0503749847412109) / 3
// Average time 0.04863433043162027

searchForPrimes(1000000, 1000050)
// 1000003 *** 0.157728970050812
// 1000033 *** 0.194114983081818
// 1000037 *** 0.196036040782928
(0.157728970050812 + 0.194114983081818 + 0.196036040782928) / 3
// Average time 0.1826266646385193

// Note the time needed to test each prime. Since the testing algorithm has an order of growth of O(n^0.5) you should expect primes around 10,000 to take about 10^0.5 times as long to test for as for primes around 1000.

/*
Times in DrRacket
;         | Actual Times |    1000 |   10000 |  100000 | 1000000 |
;    1000 |    0.00398   | 0.00398 | 0.00390 | 0.00385 | 0.00344 |
;   10000 |    0.01236   | 0.01258 | 0.01236 | 0.01219 | 0.01090 |
;  100000 |    0.03857   | 0.0398  | 0.03908 | 0.03857 | 0.03448 |
; 1000000 |    0.10905   | 0.12585 | 0.1236  | 0.12196 | 0.10905 |

Times in Swift
;         | Actual Times |    1000 |   10000 |  100000 | 1000000 |
;    1000 |    0.00450   | 0.00450 | 0.01423 | 0.04500 | 0.14230 |
;   10000 |    0.01402   | 0.00443 | 0.01402 | 0.04433 | 0.1402  |
;  100000 |    0.04863   | 0.00486 | 0.01537 | 0.04863 | 0.15378 |
; 1000000 |    0.18262   | 0.00577 | 0.01826 | 0.05774 | 0.18262 |
*/

// Do your timing data bear this out?
// Yes

// How well do the data for 100,000 and 1,000,000 support the n prediction?
// Pretty well, calculation seems to run better with a larger n

// Is your result compatible with the notion that programs on your machine run in time proportional to the number of steps required for the computation?
// Yep
