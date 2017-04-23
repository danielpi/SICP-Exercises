import Cocoa

// Exercise 2.46
// A two-dimensional vector v running from the origin to a point can be represented as a pair consisting of an x-coordinate and a y-coordinate. Implement a data abstraction for vectors by giving a constructor make-vect and a corresponding selectors xcor-vect and ycor-vect. In terms of your selectors and constructor, implement procedures add-vect, sub-vect and scale-vect that perform the operations vector addition, vector subtraction, and multiplying a vector by a scalar:

struct Pair {
    let left: Double
    let right: Double
}

func cons(_ left: Double, _ right: Double) -> Pair {
    return Pair(left: left, right: right)
}

typealias Vector = Pair
func makeVect(_ x: Double, _ y: Double) -> Vector {
    return cons(x,y)
}
let a = makeVect(1.2, 6.5)
let b = makeVect(4.4, 2.0)

func xcorVect(_ vector: Vector) -> Double {
    return vector.left
}

func ycorVect(_ vector: Vector) -> Double {
    return vector.right
}


func addVect(_ lhs: Vector, _ rhs: Vector) -> Vector {
    return makeVect(xcorVect(lhs) + xcorVect(rhs), ycorVect(lhs) + ycorVect(rhs))
}
addVect(a, b)


func subVect(_ lhs: Vector, _ rhs: Vector) -> Vector {
    return makeVect(xcorVect(lhs) - xcorVect(rhs), ycorVect(lhs) - ycorVect(rhs))
}
subVect(b, a)


func scaleVect(_ scale: Double, _ vector: Vector) -> Vector {
    return makeVect(scale * xcorVect(vector), scale * ycorVect(vector))
}
scaleVect(3, a)


