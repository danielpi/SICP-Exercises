import Cocoa

// Exercise 2.2
// Consider the problem of representing line segments in a plane. Each segment is represented as a pair of points: a starting point and an ending point. Define a constructor makeSegment and selectors startSegment and endSegment that define the representation of segments in terms of points.

// Furthermore, a point can be represented as a pair of numbers: the x coordinate and the y coordinate. Accordingly, specify a constructor makePoint and selectors xPoint and yPoint that define this representation.

// Finally, using your selectors and constructors, define a procedure midpointSegment that takes a line segment as argument and returns its midpoint.


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


let origin = makePoint(0, y: 0)
let end = makePoint(6, y: 9)
let aLine = makeSegment(origin, end)
let mid = midpointSegment(aLine)
printPoint(midpo