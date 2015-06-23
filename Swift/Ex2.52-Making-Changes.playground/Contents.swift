import Cocoa

// Exercise 2.52
// Make changes to the square limit of wave shown in Figure 2.9 by working at each of the levels described above. In particular:

// a. Add some segments to the primitive wave painter of Exercise 2.49 (to add a smile, for example)

public func wave(frame: Frame) {
    segmentsToPainter([Segment(startPoint: Point(x: 0.165, y: 0.945), endPoint: Point(x: 0.465, y: 0.665)),
        Segment(startPoint: Point(x: 0.465, y: 0.665), endPoint: Point(x: 0.465, y: 0.285)),
        Segment(startPoint: Point(x: 0.465, y: 0.455), endPoint: Point(x: 0.745, y: 0.585)),
        Segment(startPoint: Point(x: 0.465, y: 0.665), endPoint: Point(x: 0.755, y: 0.925)),
        Segment(startPoint: Point(x: 0.475, y: 0.455), endPoint: Point(x: 0.185, y: 0.615)),
        Segment(startPoint: Point(x: 0.245, y: 0.265), endPoint: Point(x: 0.685, y: 0.295)),
        Segment(startPoint: Point(x: 0.685, y: 0.295), endPoint: Point(x: 0.686, y: 0.035)),
        Segment(startPoint: Point(x: 0.685, y: 0.035), endPoint: Point(x: 0.245, y: 0.065)),
        Segment(startPoint: Point(x: 0.245, y: 0.065), endPoint: Point(x: 0.245, y: 0.265)),
        Segment(startPoint: Point(x: 0.5, y: 0.135), endPoint: Point(x: 0.245, y: 0.065)),
        Segment(startPoint: Point(x: 0.5, y: 0.135), endPoint: Point(x: 0.686, y: 0.035))])(frame)
}
draw(wave)


// b. Change the pattern constructed by cornerSplit (for example, by using only one copy of the upSplit and rightSplit images instead of two).

func cornerSplit(painter: Painter, n: Int) -> Painter {
    if n == 0 {
        return painter
    } else {
        let up = upSplit(painter, n - 1)
        let right = rightSplit(painter, n - 1)
        let topLeft = beside(up, wave)
        let bottomRight = below(right, wave)
        let corner = cornerSplit(painter, n - 1)
        
        return beside(below(topLeft, painter), below(corner, bottomRight))
    }
}
draw(cornerSplit(wave, 3))

// c. Modify the version of squareLimit that uses squareOfFour so as to assemble the corners in a different pattern. (For example you might make the big Mr. Rogers look outward from each corner of the square.)


func squareOfFour(tl: Transformer, tr: Transformer, bl: Transformer, br: Transformer) -> Transformer {
    return { painter in
        let top = beside(tl(painter), tr(painter))
        let bottom = beside(bl(painter), br(painter))
        return below(bottom, top)
    }
}

func squareLimit(painter: Painter, n: Int) -> Painter {
    let combine4 = squareOfFour(rotate180, flipVert, flipHoriz, flipHoriz)
    return combine4(cornerSplit(painter, n))
}

draw(squareLimit(wave, 4))





