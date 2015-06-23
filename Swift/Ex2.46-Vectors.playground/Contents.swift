import Cocoa

// Exercise 2.46
// A two-dimensional vector v running from the origin to a point can be represented as a pair consisting of an x-coordinate and a y-coordinate. Implement a data abstraction for vectors by giving a constructor make-vect and a corresponding selectors xcor-vect and ycor-vect. In terms of your selectors and constructor, implement procedures add-vect, sub-vect and scale-vect that perform the operations vector addition, vector subtraction, and multiplying a vector by a scalar:

struct Pair {
    let left: Double
    let right: Double
}

func cons(left: Double, right: Double) -> Pair {
    return Pair(left: left, right: right)
}

typealias Vector = Pair
func makeVect(x: Double, y: Double) -> Vector {
    return cons(x,y)
}
let a = makeVect(1.2, 6.5)
let b = makeVect(4.4, 2.0)

func xcorVect(vector: Vector) -> Double {
    return vector.left
}

func ycorVect(vector: Vector) -> Double {
    return vector.right
}


func addVect(lhs: Vector, rhs: Vector) -> Vector {
    return makeVect(xcorVect(lhs) + xcorVect(rhs), ycorVect(lhs) + ycorVect(rhs))
}
addVect(a, b)


func subVect(lhs: Vector, rhs: Vector) -> Vector {
    return makeVect(xcorVect(lhs) - xcorVect(rhs), ycorVect(lhs) - ycorVect(rhs))
}
subVect(b, a)


func scaleVect(scale: Double, vector: Vector) -> Vector {
    return makeVect(scale * xcorVect(vector), scale * ycorVect(vector))
}
scaleVect(3, a)


