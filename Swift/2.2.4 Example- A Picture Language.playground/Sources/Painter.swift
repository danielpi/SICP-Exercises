import Foundation

struct Vector {
    let x: Double
    let y: Double
}

func + (lhs: Vector, rhs: Vector) -> Vector {
    return Vector(x: lhs.x + rhs.x, y: lhs.y + rhs.y)
}
func - (lhs: Vector, rhs: Vector) -> Vector {
    return Vector(x: lhs.x - rhs.x, y: lhs.y - rhs.y)
}
func * (lhs: Double, rhs:Vector) -> Vector {
    return Vector(x: lhs * rhs.x, y: lhs * rhs.y)
}
func * (lhs: Vector, rhs: Double) -> Vector {
    return Vector(x: rhs * lhs.x, y: rhs * lhs.y)
}

typealias Point = Vector

struct Segment {
    let startPoint: Point
    let endPoint: Point
}

// Frame
struct Frame {
    let origin: Point
    let edge1: Vector
    let edge2: Vector
}

func frameCoordMap(frame: Frame) -> (Vector) -> Vector {
    return { vector in
        return frame.origin + ((vector.x * frame.edge1) + (vector.y * frame.edge2))
    }
}


// Painter Generators
typealias Painter = (Frame) -> Void

func segmentsToText(segments: [Segment]) -> Painter {
    return { frame in
        for segment in segments {
            let start = frameCoordMap(frame)(segment.startPoint)
            let end = frameCoordMap(frame)(segment.endPoint)
            print("Draw Line from \(start) to \(end)")
        }
    }
}

// Painters
func frameOutline(frame: Frame) {
    segmentsToText([Segment(startPoint: Point(x: 0, y: 0), endPoint: Point(x: 1, y: 0)),
        Segment(startPoint: Point(x: 1, y: 0), endPoint: Point(x: 1, y: 1)),
        Segment(startPoint: Point(x: 1, y: 1), endPoint: Point(x: 0, y: 1)),
        Segment(startPoint: Point(x: 0, y: 1), endPoint: Point(x: 0, y: 0))])(frame)
}


// Transformer Generator
func transformPainter(painter: Painter, origin: Point, corner1: Point, corner2: Point) -> Painter {
    return { frame in
        let m = frameCoordMap(frame)
        let newOrigin = m(origin)
        painter(Frame(origin: newOrigin, edge1: m(corner1) - newOrigin, edge2: m(corner2) - newOrigin))
    }
}


// Transformers
func beside(left: Painter, right: Painter) -> Painter {
    let splitPoint = Point(x: 0.5, y: 0)
    let paintLeft = transformPainter(left, Point(x: 0, y: 0), splitPoint, Point(x: 0, y: 1))
    let paintRight = transformPainter(right, splitPoint, Point(x: 1, y: 0), Point(x: 0.5, y: 1))
    return { frame in
        paintLeft(frame)
        paintRight(frame)
    }
}







