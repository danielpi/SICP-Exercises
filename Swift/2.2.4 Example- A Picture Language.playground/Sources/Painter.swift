import Cocoa


/*
public typealias Position = CGPoint
public typealias Distance = CGFloat
public typealias Region = Position -> Bool

public func visualizeRegion(region: Region, inRect: NSRect) -> NSImage {
    
    let squareSize: CGFloat = 500
    var imgSize = NSMakeSize(squareSize, squareSize)
    var img = NSImage(size: imgSize)
    img.lockFocus()
    
    let background = NSColor(calibratedHue: 0.0, saturation: 0.0, brightness: 0.8, alpha: 1.0)
    let truthColor = NSColor(calibratedHue: 0.8, saturation: 0.7, brightness: 0.7, alpha: 0.5)
    let falseColor = NSColor(calibratedHue: 0.0, saturation: 0.0, brightness: 0.99, alpha: 0.8)
    
    // Background
    background.setFill()
    NSRectFill(NSMakeRect(0, 0, imgSize.width, imgSize.height))
    
    let xform = NSAffineTransform()
    xform.scaleBy(squareSize / inRect.size.width)
    xform.concat()
    
    //let translation = CGAffineTransformMakeTranslation(imgSize.width / 100, imgSize.height / 500)
    
    // Point cloud
    var point = Position(x: 0, y: 0)
    srand48(1234);
    for i in 0...2000 {
        let x = CGFloat(drand48()) * inRect.size.width + inRect.origin.x //(drand48() * inRect.size.width) + inRect.origin.x
        let y = CGFloat(drand48()) * inRect.size.height + inRect.origin.y //(drand48() *
        point = Position(x: x, y: y)
        if region(point) {
            truthColor.set()
        } else {
            falseColor.set()
        }
        //NSRectFill(NSMakeRect(point.x - inRect.origin.x , point.y - inRect.origin.y, 1.0, 1.0))
        let dot = NSBezierPath(ovalInRect: NSMakeRect(point.x - inRect.origin.x - 0.2 , point.y - inRect.origin.y - 0.2, 0.4, 0.4))
        dot.fill()
    }
    
    
    img.unlockFocus()
    return img
}

var imgSize = NSMakeSize(squareSize, squareSize)
var img = NSImage(size: imgSize)
img.lockFocus()
img.unlockFocus()
return img

public func newImage() -> NSImage {
let squareSize: CGFloat = 500
var imgSize = NSMakeSize(squareSize, squareSize)
var img = NSImage(size: imgSize)
img.lockFocus()
return img
}
*/









