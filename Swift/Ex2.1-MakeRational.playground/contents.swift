import Cocoa

// Exercise 2.1
// Define a better version of makeRat that handles both positive and negative arguments. MakeRat should normalise the sign so that if the rational number is positive, both the numerator and denominator are positive, and if the rational number is negative, only the numerator is negative.

enum ConsPosition {
    case Left, Right
}

func cons<T>(a: T, b: T) -> (ConsPosition -> T) {
    func innerCons(i: ConsPosition) -> T {
        if i == .Left {
            return a;
        } else {
            return b;
        }
    }
    
    return innerCons;
}

func car<T>(innerCons: ConsPosition -> T) -> T {
    return innerCons(.Left);
}

func cdr<T>(innerCons: ConsPosition -> T) -> T {
    return innerCons(.Right);
}

typealias Rational = (ConsPosition -> Int)

func gcd(a: Int, b: Int) -> Int {
    if b == 0 {
        return abs(a)
    } else {
        return gcd(b, b: a % b)
    }
}

func makeRat(n: Int, d:Int) -> Rational {
    let g = gcd(n, b: d)
    if d < 0 {
        return cons(n/g, -d/g)
    } else {
        return cons(n/g, d/g)
    }
}

func numer(x: Rational) -> Int {
    return car(x)
}
func denom(x: Rational) -> Int {
    return cdr(x)
}

func printRat(x: Rational) {
    print("\(numer(x))/\(denom(x))")
}

func addRat(x: Rational, y: Rational) -> Rational {
    return makeRat((numer(x) * denom(y)) + (numer(y) * denom(x)), d: denom(x) * denom(y))
}
func subRat(x: Rational, y: Rational) -> Rational {
    return makeRat((numer(x) * denom(y)) - (numer(y) * denom(x)), d: denom(x) * denom(y))
}
func mulRat(x: Rational, y: Rational) -> Rational {
    return makeRat(numer(x) * numer(y), d: denom(x) * denom(y))
}
func divRat(x: Rational, y: Rational) -> Rational {
    return makeRat(numer(x) * denom(y), d: denom(x) * numer(y))
}
func isEqualRat(x: Rational, y: Rational) -> Bool {
    return (numer(x) * denom(y)) == (numer(y) * denom(x))
}


printRat(makeRat(1, d: 2))
printRat(makeRat(-1, d: 2))
printRat(makeRat(1, d: -2))
print