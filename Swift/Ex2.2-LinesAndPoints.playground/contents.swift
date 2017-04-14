import Cocoa

// Exercise 2.2
// Consider the problem of representing line segments in a plane. Each segment is represented as a pair of points: a starting point and an ending point. Define a constructor makeSegment and selectors startSegment and endSegment that define the representation of segments in terms of points.

// Furthermore, a point can be represented as a pair of numbers: the x coordinate and the y coordinate. Accordingly, specify a constructor makePoint and selectors xPoint and yPoint that define this representation.

// Finally, using your selectors and constructors, define a procedure midpointSegment that takes a line segment as argument and returns its midpoint.


enum ConsPosition {
    case Left, Right
}

func cons<T>(_ a: T, _ b: T) -> ((ConsPosition) -> T) {
    func innerCons(_ i: ConsPosition) -> T {
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

func makeSegment(_ start:@escaping Point, _ end:@escaping Point) -> Segment {
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


let origin = makePoint(0, 0)
let end = makePoint(6, 9)
let aLine = makeSegment(origin, end)
let mid = midpointSegment(aLine)
printPoint(midpointSegment(aLine))
