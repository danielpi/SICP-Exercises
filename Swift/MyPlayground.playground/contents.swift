/// Church-Turing clan ain’t nothing to func with.

/// Church encoding is a means of representing data and operators in the lambda
/// calculus.  In Swift, this means restricting functions to their fully curried
/// forms; returning blocks wherever possible.  Church Encoding of the natural
/// numbers is the most well-known form of encoding, but the lambda calculus is
/// expressive enough to represent booleans, conditionals, pairs, and lists as
/// well.  This file is an exploration of all 4 representations mentioned.
import Foundation

/// Polymorphic identity function
func id<A>(a : A) -> A {
    return a
}

/// Constant Combinator
func const<A, B>(a : A) -> B -> A {
    return { (b : B) -> A in
        return a
    }
}

/// Church encoding of the number zero.
///
/// zero :: flip const
func zero<A, B>(a : A) -> B -> B {
    return { (b : B) -> B in
        return b
    }
}

/// Returns the church encoding of any integer.
func church<A>(x : Int) -> (A -> A) -> A -> A {
    if x == 0 {
        return zero
    }
    return { (f : (A -> A)) -> A -> A in
        return { (a : A) -> A in
            return f(church(x - 1)(f)(a))
        }
    }
}

/// Returns the integer resolution of a church encoding.
///
/// This function is not tail recursive and has a tendency to smash the
/// stack with incredibly large values of church-encoded n's.
func unchurch<A>(f : ((Int -> Int) -> Int -> A)) -> A {
    return f({ (i : Int) -> Int in
        return i + 1
    })(0)
}

unchurch(church(6)) // 6
unchurch(church(25)) // 25
unchurch(church(8)) // 8

/// Natural Numbers
///
/// Church numerals are higher order functions that represent the folding of a
/// successor function 'f'.  The natural numbers begin with zero, which
/// does not apply the function, and grow monotonically with each application
/// of f.  Applications of f are encoded as a successor function.

/// Successor function for church encoded numerals.
func successor<A, B, C>(n : ((A -> B) -> C -> A)) -> (A -> B) -> C -> B {
    return { (f : (A -> B)) -> C -> B in
        return { (x : C) -> B in
            return f(n(f)(x))
        }
    }
}

/*
func zero<A, B>(a : A) -> B -> B {
    return { (b : B) -> B in
        return b
    }
}*/

func oneB<A>(f : (A -> A)) -> (A) -> A {
    return { (x : A) -> A in
        return f(x)
    }
}
unchurch(oneB)

func one<A>(f : (A -> A)) -> (A) -> A {
    return { (x : A) -> A in
        return successor(zero)(f)(x)
    }
}
unchurch(one)

func two<A>(f : (A -> A)) -> A -> A {
    return { (x : A) -> A in
        return successor(one)(f)(x)
    }
}

func three<A>(f : (A -> A)) -> A -> A {
    return { (x : A) -> A in
        return successor(two)(f)(x)
    }
}

func four<A>(f : (A -> A)) -> A -> A {
    return { (x : A) -> A in
        return successor(three)(f)(x)
    }
}

func five<A>(f : (A -> A)) -> A -> A {
    return { (x : A) -> A in
        return successor(four)(f)(x)
    }
}

func six<A>(f : (A -> A)) -> A -> A {
    return { (x : A) -> A in
        return successor(five)(f)(x)
    }
}

func seven<A>(f : (A -> A)) -> A -> A {
    return { (x : A) -> A in
        return successor(six)(f)(x)
    }
}

func eight<A>(f : (A -> A)) -> A -> A {
    return { (x : A) -> A in
        return successor(seven)(f)(x)
    }
}

func nine<A>(f : (A -> A)) -> A -> A {
    return { (x : A) -> A in
        return successor(eight)(f)(x)
    }
}

func ten<A>(f : (A -> A)) -> A -> A {
    return { (x : A) -> A in
        return successor(successor(successor(successor(successor(successor(successor(successor(successor(successor(zero))))))))))(f)(x)
    }
}

unchurch(one) // 1
unchurch(six) // 6
unchurch(seven) // 7
unchurch(ten) // 10

/// Arithmetic Operators

/// Addition of church numerals.
///
/// successor(n) beta reduces to plus(one)(n)
func plus<A, B, C>(m : (B -> A -> C)) -> (B -> C -> A) -> B -> C -> C {
    return { (n : (B -> C -> A)) -> B -> C -> C in
        return { (f : B) -> C -> C in
            return { (x : C) -> C in
                return m(f)(n(f)(x))
            }
        }
    }
}

/// Multiplication of church numerals.
func multiply<A, B, C>(m : A -> B) -> (C -> A) -> C -> B {
    return { (n : (C -> A)) -> C -> B in
        return { (f : C) -> B in
            return m(n(f))
        }
    }
}

/// Exponentiation of church numerals.
func exponentiate<A, B, C>(m : A) -> (A -> B -> C -> C) -> B -> C -> C {
    return { (n : (A -> B -> C -> C)) -> B -> C -> C in
        return { (f : B) -> C -> C in
            return { (x : C) -> C in
                return n(m)(f)(x)
            }
        }
    }
}

unchurch(plus(seven)(seven)) // 14
unchurch(multiply(three)(eight)) // 24
unchurch(exponentiate(two)(two)) // 4

/// Church Booleans
///
/// True and False in the lambda calculus are 2-argument functions that return
/// their first and second arguments respectively.
///
/// So,
///
/// True(x)(y) = x;
/// False(x)(y) = y;
///
/// Look familiar?  True is const, False is church encoded zero.

/// True
func True<A, B>(x : A) -> B -> A {
    return { (y : B) in
        return x
    }
}

/// False
func False<A, B>(a : A) -> B -> B {
    return { (b : B) -> B in
        return b
    }
}

/// Overload for Church-encoding Swift booleans.
func church<A>(x : Bool) -> A -> A -> A {
    if x == false {
        return False
    }
    return True
}

/// Overload for decoding Church-encoded boolean expressions to Swift booleans.
func unchurch<A>(f : (Bool -> Bool -> A)) -> A {
    return f(true)(false)
}


/// If-then-else.  Given an encoded boolean expression and two church numerals,
/// evaluates to the first numeral if true, and the second numeral if false.
func If<A, B, C>(b : (A -> B -> C)) -> A -> B -> C {
    return { (x : A) -> B -> C in
        return { (y : B) -> C in
            return b(x)(y)
        }
    }
}

/// Logical OR.  Given two encoded boolean expressions, returns the result of
/// logically OR'ing them together.
func or<A, B, C>(b1 : (B -> A -> C)) -> (B -> C -> A) -> B -> C -> C {
    return { (b2 : (B -> C -> A)) -> B -> C -> C in
        return { (x : B) -> C -> C in
            return { (y : C) -> C in
                return b1(x)(b2(x)(y))
            }
        }
    }
}

/// Logical XOR.  Given two encoded boolean expressions, returns the result of
/// logically XOR'ing them together.
func xor<A, B, C>(b1 : (A -> A -> C)) -> (B -> B -> A) -> B -> B -> C {
    return { (b2 : (B -> B -> A)) -> B -> B -> C in
        return { (x : B) -> B -> C in
            return { (y : B) -> C in
                return b1(b2(y)(x))(b2(x)(y))
            }
        }
    }
}

/// Logical AND.  Given two encoded boolean expressions, returns the result of
/// logically AND'ing them together.
func and<A, B, C>(b1 : (A -> B -> C)) -> (C -> B -> A) -> C -> B -> C {
    return { (b2 : (C -> B -> A)) -> C -> B -> C in
        return { (x : C) -> B -> C in
            return { (y : B) -> C in
                return b1(b2(x)(y))(y)
            }
        }
    }
}

/// Logical NAND.  Given two encoded boolean expressions, returns the result of
/// logically NAND'ing them together.
func nand<A, B, C>(b1 : (A -> B -> C)) -> (C -> B -> A) -> B -> C -> C {
    return { (b2 : (C -> B -> A)) -> B -> C -> C in
        return { (x : B) -> C -> C in
            return { (y : C) -> C in
                return b1(b2(y)(x))(x)
            }
        }
    }
}

/// Logical NOT.  Negates the church encoding of a boolean expression.
func not<A, B, C>(b : (A -> B -> C)) -> B -> A -> C {
    return { (x : B) -> A -> C in
        return { (y : A) -> C in
            return b(y)(x)
        }
    }
}

unchurch(church(false)) // false
unchurch(church(true)) // true

unchurch(or(True)(False)) // true
unchurch(xor(True)(True)) // false
unchurch(and(True)(False)) // false
unchurch(nand(False)(False)) // true

unchurch(If(and(False)(True))(ten)(two)) // 2
unchurch(or(True)(False)(one)(three)) // 1
unchurch(xor(True)(False)(nine)(three)) // 9
unchurch(and(True)(False)(five)(eight)) // 8
unchurch(nand(False)(False)(two)(three)) // 2
unchurch(not(nand(False)(False))(two)(three)) // 3

/// Pairs
///
/// Church pairs represent 2-tuples that act like a "suspended if statement".  Left
/// projection is the True function and right projection is the False function applied
/// to the tuple.

/// Constructs a pair.
func pair<A, B, C>(x : A) -> (B) -> (A -> B -> C) -> C {
    return { (y : B) -> (A -> B -> C) -> C in
        return { (f : (A -> B -> C)) -> C in
            return f(x)(y)
        }
    }
}

/// Left projection; Returns the first element of the tuple.
func first<A, B, C>(p : ((B -> A -> B) -> C)) -> C {
    return p(True)
}

/// Right-projection; Returns the second element of the tuple.
func second<A, B, C>(p : ((A -> B -> B) -> C)) -> C {
    return p(False)
}

first(pair(5)(6)) // 5
second(pair("Carlos")("Danger")) // "Danger"

/// Lists
///
/// Wikipedia calls this the "One pair as a list node" approach.  The head of the list is the
/// first element of the pair, the rest is the second element.  nil is encoded as false.  In this
/// way, it is easy to see that a list is merely a chain of functions yielding different values as
/// you burrow deeper into it.  In a sense, it is like being able to "stop" at a point in time
/// travellling along the application of the function itself.

/// Cons an element, x, onto the list and returns a new list.
func cons<A, B, C>(x : A) -> B -> (A -> B -> C) -> C {
    return { (y : B) -> (A -> B -> C) -> C in
        return { (f : (A -> B -> C)) -> C in
            return pair(x)(y)(f)
        }
    }
}

/// Returns the head of the list.
func head<A, B, C>(p : ((B -> A -> B) -> C)) -> C {
    return first(p)
}

/// Returns the tail of the list.
func tail<A, B, C>(p : ((A -> B -> B) -> C)) -> C {
    return second(p)
}

/*
/// The empty list.
func nil<A, B>(a : A) -> B -> B {
    return { (b : B) -> B in
        return False(a)(b)
    }
}
*/
/// More Arithmetic
///
/// From here it gets hairy.  One would expect to be able to write subtraction as simply a number
/// of applications of the predecessor function.  From there, division is just like the definition
/// of multiplication.  Unfortunately, Richard Statman proved that subtraction, ordering, and
/// any notion of equality over church numerals are not expressible in a typed language.
/// Oh well... it was fun while it lasted.  Stupid simply-typed lambda calculus has all the fun.

/// Predecessor function for Church Numerals.
func predecessor<A, B, C, D, E, F, G, H, I>(n : ((((E -> D -> E) -> C) -> (B -> C -> A) -> A) -> ((G -> G -> F) -> F) -> (H -> I -> I) -> E)) -> (C -> B) -> G -> E {
    return { (f : (C -> B)) -> G -> E in
        return { (x : G) -> E in
            func prefn(f : (C -> B)) -> ((E -> D -> E) -> C) -> (B -> C -> A) -> A {
                return { (p : ((E -> D -> E) -> C)) -> (B -> C -> A) -> A in
                    return { (x : (B -> C -> A)) -> A in
                        return (pair(f(first(p)))(first(p)))(x)
                    }
                }
            }
            func homopair(x : G) -> G -> (G -> G -> F) -> F {
                return { (y : G) -> (G -> G -> F) -> F in
                    return { (f : (G -> G -> F)) -> F in
                        return f(x)(y)
                    }
                }
            }
            func sec(a : H) -> I -> I {
                return { (b : I) -> I in
                    return b
                }
            }
            return (((n(prefn(f)))(homopair(x)(x)))(sec))
        }
    }
}

//unchurch(pred(zero)) Does not typecheck.  This is a very good thing.  Or maybe a terrible thing.
unchurch(predecessor(two))
unchurch(predecessor(three))
unchurch(predecessor(four))
unchurch(predecessor(five))
unchurch(predecessor(six))
 
