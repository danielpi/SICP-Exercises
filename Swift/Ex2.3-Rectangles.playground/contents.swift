import Cocoa

// Exercise 2.3
// Implement a representation for rectangles in a plane. In terms of your constructors and selectors, create procedures that comput the perimeter and the area of a given rectangle.

// Now implement a different representation for rectangles.

// Can you design your system with suitable abstraction barriers so that the same perimeter and area procedures will work using either representation?


enum ConsPosition {
    case Left, Right
}

func cons<T>(_ a: T, _ b: T) -> ((ConsPosition) -> T) {
    func innerCons(i: ConsPosition) -> T {
        if i == .Left {
            return a;
        } else {
            return b;
        }
    }
    
    return innerCons;
}

func car<T>(_ innerCons: (ConsPosition) -> T) -> T {
    return innerCons(.Left);
}

func cdr<T>(_ innerCons: (ConsPosition) -> T) -> T {
    return innerCons(.Right);
}




typealias Point = ((ConsPosition) -> Double)

func makePoint(_ x: Double, _ y: Double) -> Point {
    return cons(x, y)
}
func xPoint(_ x: Point) -> Double {
    return car(x)
}
func yPoint(_ x: Point) -> Double {
    return cdr(x)
}
func printPoint(_ x: Point) {
    print("(\(xPoint(x)),\(yPoint(x)))")
}



typealias Segment = ((ConsPosition) -> Point)

func makeSegment(_ start: @escaping Point, _ end: @escaping Point) -> Segment {
    return cons(start, end)
}
func startSegment(_ x: Segment) -> Point {
    return car(x)
}
func endSegment(_ x: Segment) -> Point {
    return cdr(x)
}
func midpointSegment(_ x: Segment) -> Point {
    func average(a: Double, _ b: Double) -> Double {
        return (a + b) / 2
    }
    return makePoint(average(a: xPoint(startSegment(x)), xPoint(endSegment(x))), average(a: yPoint(startSegment(x)), yPoint(endSegment(x))))
}


typealias Size = ((ConsPosition) -> Double)
func makeSize(width: Double, _ height: Double) -> Size {
    return cons(width, height)
}
func width(_ x: Size) -> Double {
    return car(x)
}
func height(_ x: Size) -> Double {
    return cdr(x)
}

typealias Rect = ((ConsPosition) -> Point)
func makeRect(_ origin:@escaping Point, _ size:@escaping Size) -> Rect {
    return cons(origin, size)
}
func origin(_ x: Rect) -> Point {
    return car(x)
}
func size(_ x: Rect) -> Size {
    return cdr(x)
}

func perimeter(_ rect: Rect) -> Double {
    let theWidth = width(size(rect))
    let theHeight = height(size(rect))
    return 2 * (theWidth + theHeight)
}
func area(_ rect: Rect) -> Double {
    let theWidth = width(size(rect))
    let theHeight = height(size(rect))
    return theWidth * theHeight
}

let rect1 = makeRect(makePoint(0, 0), makeSize(width: 4, 6))
perimeter(rect1)
area(rect1)

typealias Rect2 = ((ConsPosition)  -> Point)
func makeRect2(_ origin:@escaping Point, _ diagonal:@escaping Point) -> Rect {
    return cons(origin, diagonal)
}
func origin2(_ x: Rect2) -> Point {
    return car(x)
}
func diagonal(_ x: Rect2) -> Point {
    return cdr(x)
}
func size2(_ x: Rect2) -> Size {
    return makeSize(width: abs(xPoint(origin2(x)) - xPoint(diagonal(x))), abs(yPoint(origin2(x)) - yPoint(diagonal(x))))
}

func perimeter2(_ rect: Rect2) -> Double {
    let theWidth = width(size2(rect))
    let theHeight = height(size2(rect))
    return 2 * (theWidth + theHeight)
}
func area2(_ rect: Rect2) -> Double {
    let theWidth = width(size2(rect))
    let theHeight = height(size2(rect))
    return theWidth * theHeight
}

let rect2 = makeRect2(makePoint(0, 0), makeSize(width:4, 6))
perimeter2(rect2)
area2(rect2)


