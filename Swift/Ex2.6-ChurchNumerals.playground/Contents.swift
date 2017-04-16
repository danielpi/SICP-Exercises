import Cocoa

// Exercise 2.6
// In case representing pairs as procedures wasn't mind-boggling enough, consider that, in a language that can manipulate procedures, we can get by without numbers (at least insofar as nonnegative integers are concerned) by implementing 0 and the operation of adding 1 as


// I took the following from https://gist.github.com/CodaFi/b9ca5bcee6d7ea9ff158

func zero<A, B>(_ a: A) -> (B) -> B {
    return { (b: B) -> B in
        return b
    }
}

func addOne<A, B, C>(_ n: @escaping (((A) -> B) -> (C) -> A)) -> ((A) -> B) -> (C) -> B {
    return { (f: @escaping ((A) -> B)) -> (C) -> B in
        return { (x: C) -> B in
            return f(n(f)(x))
        }
    }
}

/// Returns the church encoding of any integer.
func church<A>(_ x: Int) -> ((A) -> A) -> (A) -> A {
    if x == 0 {
        return zero
    }
    return { (f:@escaping ((A) -> A)) -> (A) -> A in
        return { (a : A) -> A in
            return f(church(x - 1)(f)(a))
        }
    }
}

/// Returns the integer resolution of a church encoding.
///
/// This function is not tail recursive and has a tendency to smash the
/// stack with incredibly large values of church-encoded n's.
func unchurch<A>(_ f: (((Int) -> Int) -> (Int) -> A)) -> A {
    return f({ (i: Int) -> Int in
        return i + 1
    })(0)
}

unchurch(zero)
unchurch(addOne(zero))



// This representation is known as Church numerals, after its inventor, Alonzo Church, the logician who invented the Lambda calculus.
// Define one and two directly (not in terms of zero and add-1). (Hint: Use substitution to evaluate (add-1 zero)). 

/*
 Substitution of above
   (add-1 zero)
 - Grab the body of add-1
return { (f : (A -> B)) -> C -> B in
return { (x : C) -> B in
return f(n(f)(x))
}
}
 - Replace the formal parameters n with the body of zero
return { (f : (A -> B)) -> C -> B in
return { (x : C) -> B in
return f(zero(f)(x))
}
}

return { (f : (A -> B)) -> C -> B in
return { (x : C) -> B in
return f( return { (b : B) -> B in return b }(f)(x))
}
}

*/

func one<A>(f:@escaping ((A) -> A)) -> (A) -> A {
    return { (x : A) -> A in
        return f(x)
    }
}
unchurch(one)

func two<A>(f : (A -> A)) -> (A) -> A {
    return { (x : A) -> A in
        return f(f(x))
    }
}
unchurch(two)

// Give a direct definition of the addition procedure + (not in terms of repeated application of add-1

//typealias ChurchNumeral<A> = (A -> A) -> (A) -> A
/*func add<A, B, C>(a: (C -> B) -> C -> B, b: (C -> B) -> C -> B) -> (C -> B) -> C -> B {
    return { (f: (C -> B)) -> C -> B in
        return { (x: C) -> B in
            let bFunc = b(f)
            let aFunc = a(f)
            return aFunc(bFunc)
        }
    }
}*/

// I have no idea how this works. Straight copy.
func plus<A, B, C>(m : (B -> A -> C)) -> (B -> C -> A) -> B -> C -> C {
    return { (n : (B -> C -> A)) -> B -> C -> C in
        return { (f : B) -> C -> C in
            return { (x : C) -> C in
                return m(f)(n(f)(x))
            }
        }
    }
}
unchurch(plus(one)(two))






