import Cocoa

// Exercise 2.44
// Define the procedure upSplit used by cornerSplit. It is similar to rightSplit, except that it switches the roles of below and beside.

public struct Vector {
    public let x: Double
    public let y: Double
    
    public init(x: Double, y: Double) {
        self.x = x
        self.y = y
    }
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

func frameCoordMap(frame: Frame) -> (Vector) -> Vector {
    return { vector in
        return frame.origin + ((vector.x * frame.edge1) + (vector.y * frame.edge2))
    }
}

func segmentsToPainter(segments:[Segment]) -> Painter {
    return { frame in
        frame.dc.lockFocus()
        let lineColor = NSColor(calibratedHue: 0.0, saturation: 0.0, brightness: 0.0, alpha: 0.8)
        lineColor.setStroke()
        
        let xform = NSAffineTransform()
        xform.scaleBy(frame.dc.size.height / 1)
        xform.rotateByDegrees(-90)
        xform.translateXBy(-1, yBy: 0.0)
        xform.concat()
        
        let line = NSBezierPath()
        line.lineWidth = 1 / frame.dc.size.height
        
        for segment in segments {
            let start = frameCoordMap(frame)(segment.startPoint)
            let end = frameCoordMap(frame)(segment.endPoint)
            line.moveToPoint(NSPoint(x: start.x, y: start.y))
            line.lineToPoint(NSPoint(x: end.x, y: end.y))
            line.stroke()
        }
        frame.dc.unlockFocus()
    }
}

// Painters
public func wave(frame: Frame) {
    segmentsToPainter([Segment(startPoint: Point(x: 0.165, y: 0.945), endPoint: Point(x: 0.465, y: 0.665)),
        Segment(startPoint: Point(x: 0.465, y: 0.665), endPoint: Point(x: 0.465, y: 0.285)),
        Segment(startPoint: Point(x: 0.465, y: 0.455), endPoint: Point(x: 0.745, y: 0.585)),
        Segment(startPoint: Point(x: 0.465, y: 0.665), endPoint: Point(x: 0.755, y: 0.925)),
        Segment(startPoint: Point(x: 0.475, y: 0.455), endPoint: Point(x: 0.185, y: 0.615)),
        Segment(startPoint: Point(x: 0.245, y: 0.265), endPoint: Point(x: 0.685, y: 0.295)),
        Segment(startPoint: Point(x: 0.685, y: 0.295), endPoint: Point(x: 0.686, y: 0.035)),
        Segment(startPoint: Point(x: 0.685, y: 0.035), endPoint: Point(x: 0.245, y: 0.065)),
        Segment(startPoint: Point(x: 0.245, y: 0.065), endPoint: Point(x: 0.245, y: 0.265))])(frame)
}

// Transformer Generator
func transformPainter(painter: Painter, origin: Point, corner1: Point, corner2: Point) -> Painter {
    return { frame in
        let m = frameCoordMap(frame)
        let newOrigin = m(origin)
        painter(Frame(origin: newOrigin, edge1: m(corner1) - newOrigin, edge2: m(corner2) - newOrigin, dc: frame.dc))
    }
}


// Transformers
public func beside(left: Painter, _ right: Painter) -> Painter {
    let splitPoint = Point(x: 0.5, y: 0)
    let paintLeft = transformPainter(left, origin: Point(x: 0, y: 0), corner1: splitPoint, corner2: Point(x: 0, y: 1))
    let paintRight = transformPainter(right, origin: splitPoint, corner1: Point(x: 1, y: 0), corner2: Point(x: 0.5, y: 1))
    return { frame in
        paintLeft(frame)
        paintRight(frame)
    }
}

public func below(top: Painter, bottom: Painter) -> Painter {
    let splitPoint = Point(x: 0.0, y: 0.5)
    let paintTop = transformPainter(top, origin: Point(x:0, y:0), corner1: Point(x: 1, y: 0), corner2: splitPoint)
    let paintBot = transformPainter(bottom, origin:splitPoint, corner1:Point(x: 1, y: 0.5), corner2:Point(x: 0, y: 1))
    return { frame in
        paintTop(frame)
        paintBot(frame)
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



func upSplit(painter: Painter, n: Int) -> Painter {
    if n == 0 {
        return painter
    } else {
        let smaller = upSplit(painter, n:n - 1)
        return below(beside(smaller, smaller), bottom:painter)
    }
}
draw(upSplit(wave, n:3))
