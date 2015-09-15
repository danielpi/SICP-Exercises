import Cocoa

// Exercose 2.51
// Define the below operation for painters. below takes two painters as arguments. The resulting painter, given a frame, draws with the first painter in the bottom of the frame and with the second painter in the top. Define below in two different ways - first by writing a procedure that is analogous to the beside procedure given above, and again in terms of beside and suitable rotation operations

// First

func below(top: Painter, bottom: Painter) -> Painter {
    let splitPoint = Point(x: 0.0, y: 0.5)
    let paintTop = transformPainter(top, origin: Point(x:0, y:0), corner1: Point(x: 1, y: 0), corner2: splitPoint)
    let paintBot = transformPainter(bottom, origin: splitPoint, corner1: Point(x: 1, y: 0.5), corner2: Point(x: 0, y: 1))
    return { frame in
        paintTop(frame)
        paintBot(frame)
    }
}
draw(below(wave, bottom: wave))

// Second
func below2(top: Painter, bottom: Painter) -> Painter {
    return rotate270(besid