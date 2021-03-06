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

public func frameCoordMap(_ frame: Frame) -> (Vector) -> Vector {
    return { vector in
        return frame.origin + ((vector.x * frame.edge1) + (vector.y * frame.edge2))
    }
}

public func segmentsToPainter(_ segments:[Segment]) -> Painter {
    return { frame in
        frame.dc.lockFocus()
        let lineColor = NSColor(calibratedHue: 0.0, saturation: 0.0, brightness: 0.0, alpha: 0.8)
        lineColor.setStroke()
        
        let xform = NSAffineTransform()
        xform.scale(by: frame.dc.size.height / 1)
        xform.rotate(byDegrees: -90)
        xform.translateX(by: -1, yBy: 0.0)
        xform.concat()
        
        let line = NSBezierPath()
        line.lineWidth = 1 / frame.dc.size.height
        
        for segment in segments {
            let start = frameCoordMap(frame)(segment.startPoint)
            let end = frameCoordMap(frame)(segment.endPoint)
            line.move(to: NSPoint(x: start.x, y: start.y))
            line.line(to: NSPoint(x: end.x, y: end.y))
            line.stroke()
        }
        frame.dc.unlockFocus()
    }
}

func segmentsToText(_ segments: [Segment]) -> Painter {
    return { frame in
        for segment in segments {
            let start = frameCoordMap(frame)(segment.startPoint)
            let end = frameCoordMap(frame)(segment.endPoint)
            print("Draw Line from \(start.x),\(start.y) to \(end.x),\(end.y)")
        }
    }
}

// Painters

public func frameOutline(_ frame: Frame) {
    segmentsToPainter([Segment(startPoint: Point(x: 0, y: 0), endPoint: Point(x: 1, y: 0)),
        Segment(startPoint: Point(x: 1, y: 0), endPoint: Point(x: 1, y: 1)),
        Segment(startPoint: Point(x: 1, y: 1), endPoint: Point(x: 0, y: 1)),
        Segment(startPoint: Point(x: 0, y: 1), endPoint: Point(x: 0, y: 0))])(frame)
}

public func frameCross(_ frame: Frame) {
    segmentsToPainter([Segment(startPoint: Point(x: 0, y: 0), endPoint: Point(x: 1, y: 1)),
        Segment(startPoint: Point(x: 1, y: 0), endPoint: Point(x: 0, y: 1))])(frame)
}


public func frameDiamond(_ frame: Frame) {
    segmentsToPainter([Segment(startPoint: Point(x: 0.5, y: 0), endPoint: Point(x: 1, y: 0.5)),
        Segment(startPoint: Point(x: 1, y: 0.5), endPoint: Point(x: 0.5, y: 1)),
        Segment(startPoint: Point(x: 0.5, y: 1), endPoint: Point(x: 0, y: 0.5)),
        Segment(startPoint: Point(x: 0, y: 0.5), endPoint: Point(x: 0.5, y: 0))])(frame)
}


// Transformer Generator
public func transform(painter: @escaping Painter, origin: Point, corner1: Point, corner2: Point) -> Painter {
    return { frame in
        let m = frameCoordMap(frame)
        let newOrigin = m(origin)
        painter(Frame(origin: newOrigin, edge1: m(corner1) - newOrigin, edge2: m(corner2) - newOrigin, dc: frame.dc))
    }
}


// Transformers
public typealias Transformer = (@escaping Painter) -> Painter

public func beside(left: @escaping Painter, right: @escaping Painter) -> Painter {
    let splitPoint = Point(x: 0.5, y: 0)
    let paintLeft = transform(painter: left, origin:Point(x: 0, y: 0), corner1:splitPoint, corner2:Point(x: 0, y: 1))
    let paintRight = transform(painter: right, origin:splitPoint, corner1:Point(x: 1, y: 0), corner2:Point(x: 0.5, y: 1))
    return { frame in
        paintLeft(frame)
        paintRight(frame)
    }
}

public func below(top: @escaping Painter, bottom: @escaping Painter) -> Painter {
    let splitPoint = Point(x: 0.0, y: 0.5)
    let paintTop = transform(painter: top, origin:Point(x:0, y:0), corner1:Point(x: 1, y: 0), corner2:splitPoint)
    let paintBot = transform(painter: bottom, origin:splitPoint, corner1:Point(x: 1, y: 0.5), corner2:Point(x: 0, y: 1))
    return { frame in
        paintTop(frame)
        paintBot(frame)
    }
}

public func flipVert(_ painter: @escaping Painter) -> Painter {
    let flipped = transform(painter: painter, origin:Point(x: 0, y: 1), corner1:Point(x: 1, y: 1), corner2:Point(x: 0, y: 0))
    return { frame in
        flipped(frame)
    }
}

public func flipHoriz(_ painter: @escaping Painter) -> Painter {
    let flipped = transform(painter: painter, origin:Point(x: 1, y: 0), corner1:Point(x: 0, y: 0), corner2:Point(x: 1, y: 1))
    return { frame in
        flipped(frame)
    }
}

public func rotate90(_ painter: @escaping Painter) -> Painter {
    return transform(painter: painter, origin:Point(x: 1, y: 0), corner1:Point(x: 1, y: 1), corner2:Point(x: 0, y: 0))
}

public func rotate180(_ painter: @escaping Painter) -> Painter {
    return transform(painter: painter, origin:Point(x: 1, y: 1), corner1:Point(x: 0, y: 1), corner2:Point(x: 1, y: 0))
}

public func rotate270(_ painter: @escaping Painter) -> Painter {
    return transform(painter: painter, origin:Point(x: 0, y: 1), corner1:Point(x: 0, y: 0), corner2:Point(x: 1, y: 1))
}

public func rightSplit(_ painter: @escaping Painter, n: Int) -> Painter {
    if n == 0 {
        return painter
    } else {
        let smaller = rightSplit(painter, n:n - 1)
        return beside(left: painter, right: below(top: smaller, bottom: smaller))
    }
}


public func upSplit(_ painter: @escaping Painter, n: Int) -> Painter {
    if n == 0 {
        return painter
    } else {
        let smaller = upSplit(painter, n:n - 1)
        return below(top: beside(left: smaller, right: smaller), bottom: painter)
    }
}

public func identity(_ painter: @escaping Painter) -> Painter {
    return painter
}

// Visualisation
public func draw(_ painter: Painter) -> NSImage {
    let squareSize: CGFloat = 500
    let imgSize = NSMakeSize(squareSize, squareSize)
    let img = NSImage(size: imgSize)
    let aFrame = Frame(origin: Point(x: 0, y: 0), edge1: Vector(x: 0, y: 1), edge2: Vector(x: 1, y: 0), dc: img)
    painter(aFrame)
    return img
}



