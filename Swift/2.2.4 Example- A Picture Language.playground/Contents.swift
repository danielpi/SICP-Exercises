import Cocoa

// 2.2.4 Example: A picture language

// This section presents a simle language for drawing pictures that illustrates the power of data abstraction and closure, and also exploits higher-order procedures in an essential way. The language is designed to make it easy to experiment with patterns such as the ones in Figure 2.9, which are composed of repeated elements that are shifted and scaled. In this language, the data objects being combined are represented as procedures rather than as list structure. Just as cons, which satisfies the closure property, allowed us to easily build arbitrarily complicated list structure, the operations in this language, which also satisfy the closure property, allow us to easily build arbitrarily complicated patters.

// The picture language
// When we began our study of programming in section 1.1, we emphasized the importance of describing a language by focusing on the languages primitives, its means of combination, and its means of abstraction. We'll follow that framework here.
// Part of the elegance of this picture language is that there is only one kind of element, called a painter. A painter draws an image that is shifted and scaled to fit within a designated parallelogram-shaped frame. For example, there's a primitive painter we'll call wave that makes a crude line drawing, as shown in Figure 2.10. The actual shape of the drawing depends on the frame - all four images in figure 2.10 are produced by the same wave painter, but with respect to four different frames. Painters can be more elaborate that this: The primitive painter called rogers paints a picture of MIT's found, William Barton Rogers, as shown in Figure 2.11. The four images in figure 2.11 are drawn with respect to the same four frames as the wave images in figure 2.10.
// To combine images, we use various operations that construct new painters from given painters. For example, the beside operation takes two painters and produces a new, compound painter that draws the first painter's image in the left half of the frame and the second painter's image in the right half of the frame. Similarly, below takes two painters and produces a compound painter that draws the first painter's image below the second painter's image. Somer operations transform a single painter to produce a new painter. For example, flip-vert takes a painter and produces a painter that draws its upside-down, and flip-horiz produces a painter that draws the original painter's image left-to-right reversed.
// Figure 2.12 shows the drawing of a painter called wave4 that is built up in two stages starting from wave:



