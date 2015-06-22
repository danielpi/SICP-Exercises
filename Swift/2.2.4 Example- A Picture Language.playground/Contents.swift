import Cocoa

// 2.2.4 Example: A picture language

// This section presents a simle language for drawing pictures that illustrates the power of data abstraction and closure, and also exploits higher-order procedures in an essential way. The language is designed to make it easy to experiment with patterns such as the ones in Figure 2.9, which are composed of repeated elements that are shifted and scaled. In this language, the data objects being combined are represented as procedures rather than as list structure. Just as cons, which satisfies the closure property, allowed us to easily build arbitrarily complicated list structure, the operations in this language, which also satisfy the closure property, allow us to easily build arbitrarily complicated patters.

// The picture language
// When we began our study of programming in section 1.1, we emphasized the importance of describing a language by focusing on the languages primitives, its means of combination, and its means of abstraction. We'll follow that framework here.
// Part of the elegance of this picture language is that there is only one kind of element, called a painter. A painter draws an image that is shifted and scaled to fit within a designated parallelogram-shaped frame. For example, there's a primitive painter we'll call wave that makes a crude line drawing, as shown in Figure 2.10. The actual shape of the drawing depends on the frame - all four images in figure 2.10 are produced by the same wave painter, but with respect to four different frames. Painters can be more elaborate that this: The primitive painter called rogers paints a picture of MIT's found, William Barton Rogers, as shown in Figure 2.11. The four images in figure 2.11 are drawn with respect to the same four frames as the wave images in figure 2.10.
// To combine images, we use various operations that construct new painters from given painters. For example, the beside operation takes two painters and produces a new, compound painter that draws the first painter's image in the left half of the frame and the second painter's image in the right half of the frame. Similarly, below takes two painters and produces a compound painter that draws the first painter's image below the second painter's image. Somer operations transform a single painter to produce a new painter. For example, flip-vert takes a painter and produces a painter that draws its upside-down, and flip-horiz produces a painter that draws the original painter's image left-to-right reversed.
// Figure 2.12 shows the drawing of a painter called wave4 that is built up in two stages starting from wave:



func wave2(frame: Frame) {
    beside(wave, flipVert(wave))(frame)
}
func wave4(frame: Frame) {
    below(wave2, wave2)(frame)
}


draw(wave)
draw(wave2)
draw(wave4)

// In building up a complex image in this manner we are exploiting the fact that painters are closed under the language's means of combination. The beside and below of two painters is itself a painter; therefore, we can use it as an element in making more complex painters. As with building up list strucutre using cons, the closure of our data under the means of combination is crucial to the ability to create complex structures while using only a few operations.
// Once we can combine painters, we would like to be able to abstract typical patterns of combining painters. We will implement the painter operations as Scheme procedures. This means that we don't need a special abstraction mechanism in the picture language: Since the means of combination are ordinary Scheme procedures, we automatically have the capability to do anything with painter operations that we can do with procedures. For example, we can abstract the pattern in wave4 as

func flippedPairs(painter: Painter) -> Painter {
    let painter2 = beside(painter, flipVert(painter))
    return below(painter2, painter2)
}
draw(flippedPairs(wave))

func wave4b(frame: Frame) {
    flippedPairs(wave)(frame)
}
draw(wave4b)

// We can also define recursive operations. Here's one that makes painters split and branch towards the right as shown in Figure 2.13 and Figure 2.14

func rightSplit(painter: Painter, n: Int) -> Painter {
    if n == 0 {
        return painter
    } else {
        let smaller = rightSplit(painter, n - 1)
        return beside(painter, below(smaller, smaller))
    }
}
draw(rightSplit(wave, 2))

func upSplit(painter: Painter, n: Int) -> Painter {
    if n == 0 {
        return painter
    } else {
        let smaller = upSplit(painter, n - 1)
        return below(beside(smaller, smaller), painter)
    }
}

// We can produce balanced patterns by branching upwards as well as towards the right.

func cornerSplit(painter: Painter, n: Int) -> Painter {
    if n == 0 {
        return painter
    } else {
        let up = upSplit(painter, n - 1)
        let right = rightSplit(painter, n - 1)
        let topLeft = beside(up, up)
        let bottomRight = below(right, right)
        let corner = cornerSplit(painter, n - 1)
        
        return beside(below(topLeft, painter), below(corner, bottomRight))
    }
}
draw(cornerSplit(wave, 5))

// By placing four copies of a cornerSplit appropriately, we obtain a pattern called square-Limit, whose application to wave and rogers is shown in Figure 2.9.

func squareLimit(painter: Painter, n: Int) -> Painter {
    let quarter = cornerSplit(painter, n)
    let half = beside(flipHoriz(quarter), quarter)
    return below(half, flipVert(half))
}
draw(squareLimit(wave, 4))


// Higher-order Operations
// In addition to abstracting patterns of combining painters, we can work at a higher level, abstracting patterns of combining painter operations. That is, we can view the painter operations as elements to manipulate and can write means of combination for these elements - procedures that take painter operations as arguments and create new painter operations.
// For example, flippedPairs and squareLimit each arrange four copies of a painter's image in a square pattern; they differ only in how they orient the copies. One way to abstract this pattern of painter combination is with the following procedure, which takes four one-argument painter operations and produces a painter operation that transforms a given painter with those four operations and arranges the results in a square. tl, tr, bl, and br are the transformations to aply to the top left copy, the top right copy, the bottem left copy and the bottom right copy, respectively.

func squareOfFour(tl: Transformer, tr: Transformer, bl: Transformer, br: Transformer) -> Transformer {
    return { painter in
        let top = beside(tl(painter), tr(painter))
        let bottom = beside(bl(painter), br(painter))
        return below(bottom, top)
    }
}

func identity(painter: Painter) -> Painter {
    return painter
}

// Then flippedPairs can be defined in terms of squareOfFour as follows:

func flippedPairs2(painter: Painter) -> Painter {
    let combine4 = squareOfFour(identity, flipVert, identity, flipVert)
    return combine4(painter)
}
draw(flippedPairs2(wave))

// and squareLimit can be expressed as

func rotate180(painter: Painter) -> Painter {
    return transformPainter(painter, Point(x: 1, y: 1), Point(x: 0, y: 1), Point(x: 1, y: 0))
}

func squareLimit2(painter: Painter, n: Int) -> Painter {
    let combine4 = squareOfFour(rotate180, flipVert, flipHoriz, identity)
    return combine4(cornerSplit(painter, n))
}
draw(squareLimit2(wave, 4))


// Frames










