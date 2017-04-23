import Cocoa


struct Vector {
    let x: Double
    let y: Double
}
typealias Point = Vector
typealias Frame = [Vector]

struct Frame2 {
    let origin: Point
    let edge1: Vector
    let edge2: Vector
}

// Exercise 2.47
// Here are two possible constructors for frames:

func makeFrame(origin: Point, edge1: Vector, edge2: Vector) -> Frame {
    return [origin, edge1, edge2]
}
func makeFrame2(origin: Point, edge1: Vector, edge2: Vector) -> Frame2 {
    return Frame2(origin: origin, edge1: edge1, edge2: edge2)
}
// ALright the version above isn't the same as from the book but I am over using cons to represent everything, really doesn't work in Swift.


// For each constructor supply the appropriate selectors to produce an implementation for frames.

func frameOrigin(frame: Frame) -> Point {
    return frame[0]
}
func frameEdge1(frame: Frame) -> Vector {
    return frame[1]
}
func frameEdge2(frame: Frame) -> Vector {
    return frame[2]
}


func frame2Origin(frame: Frame2) -> Point {
    return frame.origin
}
func frame2Edge1(frame: Frame2) -> Vector {
    return frame.edge1
}
func frame2Edge2(frame: Frame2) -> Vector {
    return frame.edge2
}

print("done")



