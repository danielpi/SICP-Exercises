import Cocoa

// Exercise 2.5
// Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations if we represent the pair a and b as the integer that is the product 2^a x 3^b. Give the corresponding definitions of the procedures cons, car and cdr

func ipow(base: Int, _ exp: Int) -> Int {
    if exp == 1 {
        return base
    } else {
        return base * ipow(base, exp - 1)
    }
}

let a = ipow(2, 3)
print("\(a)")

func cons(a: Int, _ b: Int) -> Int {
    return ipow(2, a) * ipow(3, b)
}

func car(z: Int) -> Int {
    var inner: (Int, Int) -> Int = { _, _ in return 0 }
    inner = { (value, count) -> Int in
            if (value % 2) == 1 {
                return count
            } else {
                return inner(value / 2, count + 1)
            }
        }
    return inner(z, 0)
}

func cdr(z: Int) -> Int {
    var inner: (Int, Int) -> Int = { _, _ in return 0 }
    inner = { (value, count) -> Int in
        if (value % 3) != 0 {
            return count
        } else {
            return inner(value / 3, count + 1)
        }
    }
    return inner(z, 0)
}

let aPair = cons(3, 4)
car(aPair)
cdr(aPair)

