import Cocoa

// Exercise 2.49
// Use segmentsToPainter to define the following primitive painters;

// a. The painter that draws the outline of the designated frame.
// b. The painter that draws an "X" by connecting opposite corners of the frame.
// c. The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
// d. the wave painter.



func segmentsToPainter(_ segments:[Segment]) -> Painter {
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


// Painters

func frameOutline(_ frame: Frame) {
    segmentsToPainter([Segment(startPoint: Point(x: 0, y: 0), endPoint: Point(x: 1, y: 0)),
        Segment(startPoint: Point(x: 1, y: 0), endPoint: Point(x: 1, y: 1)),
        Segment(startPoint: Point(x: 1, y: 1), endPoint: Point(x: 0, y: 1)),
        Segment(startPoint: Point(x: 0, y: 1), endPoint: Point(x: 0, y: 0))])(frame)
}
draw(frameOutline)


func frameCross(_ frame: Frame) {
    segmentsToPainter([Segment(startPoint: Point(x: 0, y: 0), endPoint: Point(x: 1, y: 1)),
                       Segment(startPoint: Point(x: 1, y: 0), endPoint: Point(x: 0, y: 1))])(frame)
}
draw(frameCross)


func frameDiamond(_ frame: Frame) {
    segmentsToPainter([Segment(startPoint: Point(x: 0.5, y: 0), endPoint: Point(x: 1, y: 0.5)),
        Segment(startPoint: Point(x: 1, y: 0.5), endPoint: Point(x: 0.5, y: 1)),
        Segment(startPoint: Point(x: 0.5, y: 1), endPoint: Point(x: 0, y: 0.5)),
        Segment(startPoint: Point(x: 0, y: 0.5), endPoint: Point(x: 0.5, y: 0))])(frame)
}
draw(frameDiamond)


public func wave(_ frame: Frame) {
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
draw(wave)

print("Is this working?")

