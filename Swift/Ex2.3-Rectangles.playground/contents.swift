import Cocoa

// Exercise 2.3
// Implement a representation for rectangles in a plane. In terms of your constructors and selectors, create procedures that comput the perimeter and the area of a given rectangle.

// Now implement a different representation for rectangles.

// Can you design your system with suitable abstraction barriers so that the same perimeter and area procedures will work using either representation?


enum ConsPosition {
    case Left, Right
}

func cons<T>(a: T, b: T) -> (ConsPosition -> T) {
    func innerCons(i: ConsPosition) -> T {
        if i == .Left {
            return a;
        } else {
            return b;
        }
    }
    
    return innerCons;
}

func car<T>(innerCons: ConsPosition -> T) -> T {
    return innerCons(.Left);
}

func cdr<T>(innerCons: ConsPosition -> T) -> T {
    return innerCons(.Right);
}




typealias Point = (ConsPosition -> Double)

func makePoint(x: Double, y: Double) -> Point {
    return cons(x, b: y)
}
func xPoint(x: Point) -> Double {
    return car(x)
}
func yPoint(x: Point) -> Double {
    return cdr(x)
}
func printPoint(x: Point) {
    print("(\(xPoint(x)),\(yPoint(x)))")
}



typealias Segment = (ConsPosition -> Point)

func makeSegment(start: Point, end: Point) -> Segment {
    return cons(start, b: end)
}
func startSegment(x: Segment) -> Point {
    return car(x)
}
func endSegment(x: Segment) -> Point {
    return cdr(x)
}
func midpointSegment(x: Segment) -> Point {
    func average(a: Double, b: Double) -> Double {
        return (a + b) / 2
    }
    return makePoint(average(xPoint(startSegment(x)), b: xPoint(endSegment(x))), y: average(yPoint(startSegment(x)), b: yPoint(endSegment(x))))
}


typealias Size = (ConsPosition -> Double)
func makeSize(width: Double, height: Double) -> Size {
    return cons(width, b: height)
}
func width(x: Size) -> Double {
    return car(x)
}
func height(x: Size) -> Double {
    return cdr(x)
}

typealias Rect = (ConsPosition -> Point)
func makeRect(origin: Point, size: Size) -> Rect {
    return cons(origin, b: size)
}
func origin(x: Rect) -> Point {
    return car(x)
}
func size(x: Rect) -> Size {
    return cdr(x)
}

func perimeter(rect: Rect) -> Double {
    let theWidth = width(size(rect))
    let theHeight = height(size(rect))
    return 2 * (theWidth + theHeight)
}
func area(rect: Rect) -> Double {
    let theWidth = width(size(rect))
    let theHeight = height(size(rect))
    return theWidth * theHeight
}

let rect1 = makeRect(makePoint(0, y: 0), size: makeSize(4, height: 6))
perimeter(rect1)
area(rect1)

typealias Rect2 = (ConsPosition -> Point)
func makeRect2(origin: Point, diagonal: Point) -> Rect {
    return cons(origin, b: diagonal)
}
func origin2(x: Rect2) -> Point {
    return car(x)
}
func diagonal(x: Rect2) -> Point {
    return cdr(x)
}
func size2(x: Rect2) -> Size {
    return makeSize(abs(xPoint(origin2(x)) - xPoint(diagonal(x))), height: abs(yPoint(origin2(x)) - yPoint(diagonal(x))))
}

func perimeter2(rect: Rect2) -> Double {
    let theWidth = width(size2(rect))
    let theHeight = height(size2(rect))
    return 2 * (theWidth + theHeight)
}
func area2(rect: Rect2) -> Double {
    let theWidth = width(size2(rect))
    let theHeight = height(size2(rect))
    return theWidth * theHeight
}

let rect2 = makeRect2(makePoint(0, y: 0),