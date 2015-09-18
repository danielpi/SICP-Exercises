import Cocoa

// Exercise 1.25

// What if we change the expMod function to the following.

func expMod(base: Int, _ exp: Int, _ m: Int) -> Int {
    print(".")
    switch true {
    case exp == 0:
        return 1
    case isEven(exp):
        return ((expMod(base, exp / 2, m)) % m) * ((expMod(base, exp / 2, m)) % m)
    default:
        return (base * expMod(base, exp - 1, m)) % m
    }
}

func fastExpt(b: Int, _ n: Int) -> Int {
    switch true {
    case n == 0:
        return 1
    case isEven(n):
        return square(fastExpt(b, n / 2))
    default:
        return b * fastExpt(b, n - 1)
    }
}

func isEven(n: Int) -> Bool {
    return (n % 2) == 0
}
func square(x: Int) -> Int {
    return x * x
}
func dividesWithNoRemainder(a: Int, _ b: Int) -> Bool {
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

timedPrimeTest(11)
//timedPrimeTest(1009)
//timedPrimeTest(10007)
//timedPrimeTest(100003)
//timedPrimeTest(1000003)

/*
Using Original expMod with O(log(n)) growth
    n       expModCount Time
    11      75          0.5796
    1009    175         1.3
    10007   225         1.56
    100003  255         1.73
    1000003 295         2.0

Using the modified expMod
    n       expModCount Time
    11      283         1.5
    1009    Crashed
    10007
    100003
    1000003
*/

// The new expMod causes expMod to be called twice whenever the square function was called previously. This evidently leads to the new expMod have O(n) growth. I can't say that I understand the reason why.
