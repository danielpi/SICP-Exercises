import Cocoa

// Exercise 2.50
// Define the transformation flipHoriz which flips painters horizontally, and transformations that rotate painters counterclockwise by 180 degrees and 270 degrees

func flipHoriz(painter: Painter) -> Painter {
    let flipped = transformPainter(painter, origin: Point(x: 1, y: 0), corner1: Point(x: 0, y: 0), corner2: Point(x: 1, y: 1))
    return { frame in
        flipped(frame)
    }
}
draw(flipHoriz(wave))

func rotate180(painter: Painter) -> Painter {
    return transformPainter(painter, origin: Point(x: 1, y: 1), corner1: Point(x: 0, y: 1), corner2: Point(x: 1, y: 0))
}
draw(rotate180(wave))

func rotate270(painter: Painter) -> Painter {
    return transformPainter(painter, origin: Point(x: 0, y: 1), co