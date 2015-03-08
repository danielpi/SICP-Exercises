import Cocoa

// Chapter 2 - Building Abstractions with Data

// Chapter 1 was focused on computational processes and the role of procedures in program design. We saw how to use primitive data (numbers) and primitive operations (arithmetic operations), how to combine procedures to form compond procedures through composition, continionals adn the use of parameters.

// In chapter two we are going to look at more complex data. Chapter 1 was about building abstractions by combining procedures to form compound procedures, we turn in this chapter to another key aspect of any programming language: the means it provides for building abstractions by combining data objects to form combound data

// Why do we want combound data in a programming language?
// - to elevate the conceptual level at which we can design our programs
// - to increase the modularity of our designs
// - enhance expressive power


// Section 2.1 - Introduction to Data Abstraction
// In sections 1.1.8, we noted that a procedure used as an element in creating a more complex procedure could be regarded not only as a collection of particular operations but also as a procedural abstraction. So details of how the procedure was implemented could be suppressed. In other words, we could make an abstraction that would seperate the way the procedure would be used from the details of how the procedure was implemented. The analogous notion for compound data is called data abstraction. Which enables us to isolate how a compound data object is used from the details of how it is constracted from more primitive data objects.

// The basic idea of data abstraction is to structure the programs that are to use compound data objects so that they operate on "abstract data." Our programs should use data in such a way as to make no assumptions about the data that are not strictly nexessary for performing the task at hand. At the same time a "concrete" data representation is defined independent of the programs that use the data. The interface between these two parts of our system will be a set of procedures, called selectors and constructors, that implement the abstract data in terms of the concrete representations.

// 2.1.1 Example: Arithmetic Operations for Rational Numbers
// Pairs, To enable us to implement the concrete level of our data abstraction, our language provides a compound stracture called a pair, which can be constructed with the primitive procedure cons. This procedure takes two arguments and returns a compound data object that contains the two arguments as parts. Given a pair, we can extract the parts using the primitive procedures car and cdr. Thus we can use cons, car and cdr as follows

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


let x = cons(1, 2)
car(x)
cdr(x)

let y = cons(3, 4)
let z = cons(x, y)
car(car(z))
car(cdr(z))

// Pairs can be used as general purpose building blocks to create all sorts of complex data structures. The single compount-data primitive pair, implemented by the procedures cons, car, and cdr, is the only glue we need. Data objects constructed from pairs are called list-structured data.

/*
(define (cons x y)
(lambda (m) (m x y)))
(define (car z)
(z (lambda (p q) p)))
(define (cdr z)
(z (lambda (p q) q)))
*/

// Representing rational numbers
// Pairs offer a natural way to complete the rational-number system. Simply represent a rational number as a pair of two integers: a numerator and a denominator. Then makeRat, numer and denom are readily implemented as follows

typealias Rational = (ConsPosition -> Int)

func makeRat1(n: Int, d:Int) -> Rational {
    return cons(n,d)
}
func numer(x: Rational) -> Int {
    return car(x)
}
func denom(x: Rational) -> Int {
    return cdr(x)
}

func printRat(x: Rational) {
    println("\(numer(x))/\(denom(x))")
}

func addRat1(x: Rational, y: Rational) -> Rational {
    return makeRat1((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
}
func subRat1(x: Rational, y: Rational) -> Rational {
    return makeRat1((numer(x) * denom(y)) - (numer(y) * denom(x)), denom(x) * denom(y))
}
func mulRat1(x: Rational, y: Rational) -> Rational {
    return makeRat1(numer(x) * numer(y), denom(x) * denom(y))
}
func divRat1(x: Rational, y: Rational) -> Rational {
    return makeRat1(numer(x) * denom(y), denom(x) * numer(y))
}
func isEqualRat(x: Rational, y: Rational) -> Bool {
    return (numer(x) * denom(y)) == (numer(y) * denom(x))
}

let oneHalf = makeRat1(1, 2)
printRat(oneHalf)
let oneThird = makeRat1(1, 3)
printRat(addRat1(oneHalf, oneThird))
printRat(mulRat1(oneHalf, oneThird))
printRat(addRat1(oneThird, oneThird))

// The final example shows that our rational-number implementation does not reduce numbers to lowest terms. We can remedy this by changing makeRat. 

func gcd(a: Int, b: Int) -> Int {
    if b == 0 {
        return abs(a)
    } else {
        return gcd(b, a % b)
    }
}

func makeRat2(n: Int, d:Int) -> Rational {
    let g = gcd(n, d)
    return cons(n/g, d/g)
}

func addRat2(x: Rational, y: Rational) -> Rational {
    return makeRat2((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
}

printRat(addRat2(oneThird, oneThird))


