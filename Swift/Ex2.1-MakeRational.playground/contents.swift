import Cocoa

// Exercise 2.1
// Define a better version of makeRat that handles both positive and negative arguments. MakeRat should normalise the sign so that if the rational number is positive, both the numerator and denominator are positive, and if the rational number is negative, only the numerator is negative.

enum ConsPosition {
    case Left, Right
}

func cons<T>(_ a: T, _ b: T) -> ((ConsPosition) -> T) {
    func innerCons(i: ConsPosition) -> T {
        if i == .Left {
            return a;
        } else {
            return b;
        }
    }
    
    return innerCons;
}

func car<T>(_ innerCons: (ConsPosition) -> T) -> T {
    return innerCons(.Left);
}

func cdr<T>(_ innerCons: (ConsPosition) -> T) -> T {
    return innerCons(.Right);
}

typealias Rational = ((ConsPosition) -> Int)

func gcd(_ a: Int, _ b: Int) -> Int {
    if b == 0 {
        return abs(a)
    } else {
        return gcd(b, a % b)
    }
}

func makeRat(_ n: Int, _ d:Int) -> Rational {
    let g = gcd(n, d)
    if d < 0 {
        return cons(n/g, -d/g)
    } else {
        return cons(n/g, d/g)
    }
}

func numer(_ x: Rational) -> Int {
    return car(x)
}
func denom(_ x: Rational) -> Int {
    return cdr(x)
}

func printRat(_ x: Rational) {
    print("\(numer(x))/\(denom(x))")
}

func addRat(_ x: Rational, _ y: Rational) -> Rational {
    return makeRat((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
}
func subRat(_ x: Rational, _ y: Rational) -> Rational {
    return makeRat((numer(x) * denom(y)) - (numer(y) * denom(x)), denom(x) * denom(y))
}
func mulRat(_ x: Rational, _ y: Rational) -> Rational {
    return makeRat(numer(x) * numer(y), denom(x) * denom(y))
}
func divRat(_ x: Rational, _ y: Rational) -> Rational {
    return makeRat(numer(x) * denom(y), denom(x) * numer(y))
}
func isEqualRat(_ x: Rational, _ y: Rational) -> Bool {
    return (numer(x) * denom(y)) == (numer(y) * denom(x))
}


printRat(makeRat(1, 2))
printRat(makeRat(-1, 2))
printRat(makeRat(1, -2))
printRat(makeRat(-1, -2))




