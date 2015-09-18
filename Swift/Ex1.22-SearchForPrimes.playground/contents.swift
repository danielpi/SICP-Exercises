import Cocoa

// Exercise 1.22
func isEven(n: Int) -> Bool {
    return (n % 2) == 0
}
func square(x: Int) -> Int {
    return x * x
}
func dividesWithNoRemainder(a: Int, _ b: Int) -> Bool {
    return  a % b == 0
}
// dividesWithNoRemainder(10, 2)

func findDivisor(n: Int, _ testDivisor: Int) -> Int {
    switch true {
    case square(testDivisor) > n:
        return n
    case dividesWithNoRemainder(n, testDivisor):
        return testDivisor
    default:
        return findDivisor(n, testDivisor + 1)
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
// timedPrimeTest(7013)

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
// 1009 *** 0.23639303445816
// 1013 *** 0.227622985839844
// 1019 *** 0.216214001178741
(0.23639303445816 + 0.227622985839844 + 0.216214001178741) / 3
// Average time 0.00398

searchForPrimes(10000, 10050)
// 10007 *** 0.727922022342682
// 10009 *** 0.748418986797333
// 10037 *** 0.735283017158508
(0.727922022342682 + 0.748418986797333 + 0.735283017158508) / 3
// Average time 0.012369

searchForPrimes(100000, 100050)
// 100003 *** 2.24567699432373
// 100019 *** 2.0843819975853
// 100043 *** 2.03399205207825
(2.24567699432373 + 2.0843819975853 + 2.03399205207825) / 3
// Average time 0.03857

searchForPrimes(1000000, 1000050)
// 1000003 *** 6.36476397514343
// 1000033 *** 6.54135400056839
// 1000037 *** 6.34981900453568
(6.36476397514343 + 6.54135400056839 + 6.34981900453568) / 3
// Average time 0.109049

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
;    1000 |    0.22674   | 0.22674 | 0.23312 | 0.21213 | 0.20297 |
;   10000 |    0.73720   | 0.71702 | 0.73720 | 0.67082 | 0.64186 |
;  100000 |    2.12135   | 2.26743 | 2.33125 | 2.12135 | 2.02975 |
; 1000000 |    6.41864   | 7.17025 | 7.37208 | 6.70829 | 6.41864 |
*/

// Do your timing data bear this out?
// Yes

// How well do the data for 100,000 and 1,000,000 support the n prediction?
// Pretty well, calculation seems to run better with a larger n

// Is your result compatible with the notion that programs on your machine run in time proportional to the number of steps required for the computation?
// Yep
