import Cocoa

// Exercise 2.50
// Define the transformation flipHoriz which flips painters horizontally, and transformations that rotate painters counterclockwise by 180 degrees and 270 degrees

func flipHoriz(painter: @escaping Painter) -> Painter {
    let flipped = transformPainter(painter: painter, origin:Point(x: 1, y: 0), corner1:Point(x: 0, y: 0), corner2:Point(x: 1, y: 1))
    return { frame in
        flipped(frame)
    }
}
draw(painter: flipHoriz(painter: wave))

func rotate180(painter: @escaping Painter) -> Painter {
    return transformPainter(painter: painter, origin:Point(x: 1, y: 1), corner1:Point(x: 0, y: 1), corner2:Point(x: 1, y: 0))
}
draw(painter: rotate180(painter: wave))

func rotate270(painter: @escaping Painter) -> Painter {
    return transformPainter(painter: painter, origin:Point(x: 0, y: 1), corner1:Point(x: 0, y: 0), corner2:Point(x: 1, y: 1))
}
draw(painter: rotate270(painter: wave))
