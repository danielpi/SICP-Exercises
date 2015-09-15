import Cocoa

public struct Vector {
    public let x: Double
    public let y: Double
    
    public init(x: Double, y: Double) {
        self.x = x
        self.y = y
    }
}

public func + (lhs: Vector, rhs: Vector) -> Vector {
    return Vector(x: lhs.x + rhs.x, y: lhs.y + rhs.y)
}
public func - (lhs: Vector, rhs: Vector) -> Vector {
    return Vector(x: lhs.x - rhs.x, y: lhs.y - rhs.y)
}
public func * (lhs: Double, rhs:Vector) -> Vector {
    return Vector(x: lhs * rhs.x, y: lhs * rhs.y)
}
public func * (lhs: Vector, rhs: Double) -> Vector {
    return Vector(x: rhs * lhs.x, y: rhs * lhs.y)
}

public typealias Point = Vector

public struct Segment {
    public let startPoint: Point
    public let endPoint: Point
    
    public init(startPoint: Point, endPoint: Point) {
        self.startPoint = startPoint
        self.endPoint = endPoint
    }
}

// Frame
public struct Frame {
    public let origin: Point
    public let edge1: Vector
    public let edge2: Vector
    public var dc: NSImage
    
    public init(origin: Point, edge1: Vector, edge2: Vector, dc: NSImage) {
        self.origin = origin
        self.edge1 = edge1
        self.edge2 = edge2
        self.dc = dc
    }
}


// Painter Generators
public typealias Painter = (Frame) -> Void

public func frameCoordMap(frame: Frame) -> (Vector) -> Vector {
    return { vector in
        return frame.origin + ((vector.x * frame.edge1) + (vector.y * frame.edge2))
    }
}

public func draw(painter: Painter) -> NSImage {
    let squareSize: CGFloat = 500
    let imgSize = NSMakeSize(squareSize, squareSize)
    let img = NSImage(size: imgSize)
    let aFrame = Frame(origin: Point(x: 0, y: 0), edge1: Vector(x: 0, y: 1), edge2: Vector(x: 1, y: 0), dc: img)
    painter(aFrame)
    return img
}
