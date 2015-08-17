#lang racket


(require racket/draw)
(require racket/gui)


; 2.2.4 Example: A Picture Language
(display "2.2.4 Example: A Picture Language") (newline)

; First up is some code that is required to make the rest of this section work. Skip over it and 
; follow along with the text if you like. I found it much easier to follow along once the examples
; started working.



; Vector
(define (make-vect x y)
  (cons x y))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))
(define (add-vect vec1 vec2)
  (make-vect (+ (xcor-vect vec1) (xcor-vect vec2))
             (+ (ycor-vect vec1) (ycor-vect vec2))))
(define (sub-vect vec1 vec2)
  (make-vect (- (xcor-vect vec1) (xcor-vect vec2))
             (- (ycor-vect vec1) (ycor-vect vec2))))
(define (scale-vect scale vect)
  (make-vect (* scale (xcor-vect vect))
             (* scale (ycor-vect vect))))

; Segment
(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

; Frame
(define (make-frame origin edge1 edge2 dc)
  (list origin edge1 edge2 dc))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))
(define (dc frame)
  (cadddr frame))


(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))


; Painter Generators

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (let ((start ((frame-coord-map frame) (start-segment segment)))
             (end ((frame-coord-map frame) (end-segment segment))))
         (send (dc frame) draw-line 
               (xcor-vect start)
               (ycor-vect start)
               (xcor-vect end)
               (ycor-vect end))))
     segment-list)))

(define (segments->text segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (let ((start ((frame-coord-map frame) (start-segment segment)))
             (end ((frame-coord-map frame) (end-segment segment))))
         (fprintf (current-output-port) "Draw Line:StartX:~a Y:~a EndX:~a Y:~a\n" (xcor-vect start) (ycor-vect start) (xcor-vect end) (ycor-vect end))))
     segment-list)))
         

; Painters
(define (frame-outline frame)
  ((segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 0))
                              (make-segment (make-vect 1 0) (make-vect 1 1))
                              (make-segment (make-vect 1 1) (make-vect 0 1))
                              (make-segment (make-vect 0 1) (make-vect 0 0)))) frame))

(define (frame-cross frame)
  ((segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                           (make-segment (make-vect 1 0) (make-vect 0 1)))) frame))

(define (frame-diamond frame)
 ((segments->painter (list (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                             (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
                             (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
                             (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))) frame))

(define (wave frame)
  ((segments->painter (list (make-segment (make-vect 0.165 0.945) (make-vect 0.465 0.665)) ; Left Leg
                           (make-segment (make-vect 0.465 0.665) (make-vect 0.465 0.285)) ; Body
                           (make-segment (make-vect 0.465 0.455) (make-vect 0.745 0.585)) ; Right arm
                           (make-segment (make-vect 0.465 0.665) (make-vect 0.755 0.925)) ;
                           (make-segment (make-vect 0.475 0.455) (make-vect 0.185 0.615)) ; ?
                           (make-segment (make-vect 0.245 0.265) (make-vect 0.685 0.295)) ; Head - Bot
                           (make-segment (make-vect 0.685 0.295) (make-vect 0.685 0.035)) ;
                           (make-segment (make-vect 0.685 0.035) (make-vect 0.245 0.065)) ;
                           (make-segment (make-vect 0.245 0.065) (make-vect 0.245 0.265)))) frame))

; Transformer Generator

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)
                  (dc frame)))))))


; Transformers
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)   
                     (make-vect 0.0 0.0)   
                     (make-vect 1.0 1.0))) 

(define (shrink-to-upper-right painter)
  (transform-painter painter 
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5) 
                     (make-vect 0.5 1.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.35 0.35)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left (transform-painter painter1 (make-vect 0.0 0.0) split-point (make-vect 0.0 1.0)))
          (paint-right (transform-painter painter2 split-point (make-vect 1.0 0.0) (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-top (transform-painter painter1 (make-vect 0.0 0.0) (make-vect 1.0 0.0) split-point))
          (paint-bot (transform-painter painter2 split-point (make-vect 1.0 0.5) (make-vect 0.0 1.0) )))
      (lambda (frame)
        (paint-top frame)
        (paint-bot frame)))))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))


  
(display "Wave") (newline)
(define target-1 (make-bitmap 100 100))
(define DC-1 (new bitmap-dc% [bitmap target-1]))
(define frame-1 (make-frame (make-vect 0 0) (make-vect 50.0 0.0) (make-vect 0.0 50.0) DC-1))
(make-object image-snip% target-1)
(wave frame-1)

; This section presents a simple language for drawing pictures that illustrates the power of data
; abstractionand closure, and also exploits higher-order procedures in an essential way. The language
; is designed to make it easy to experiment with patterns, which are composed of repeated elements
; that are shifted and scaled. In this language, the data objects being combined are represented as
; procedures rather than as list structure. Just as cons , which satisfies the closure property, 
; allowed us to easily build arbitrarily complicated list structure, the operations in this languag
; whic also satisfy the closure property, allow us to build arbitrarily complicated patterns.

; The picture language
; When we began our study of programming in Section 1.1, we emphasized the importance of describing
; a language by focusing on the language's primitives, its means of combination, and its means of
; abstraction. We'll follow that framework here.

; Part of the elegance of this picture language is that there is only one kind of element, called
; a painter. A painter draws an image that is shifted and scaled to fit within a designated 
; parallelogram-shaped frame. For example, there's a primitive painter we'll call wave that makes
; a crude line drawing. The actual shape of the drawing depends on the frame - all four images in
; figure 2.10 are produced by the same wave painter, but with respect to four different frames.
; Painters can be more elaborate than this: The primitive painter called rogers paints a picture of 
; MIT's founder, William Barton Rogers, as shown in Figure 2.11. The four images in figure 2.11 are 
; drawn with respect to the same four frames as the wave images in figure 2.10.

; To combine images, we use various operations that construct new painters from given painters. For
; example, the beside operation takes two painters and produces a new, compound painter that draws
; the first painter's image in the left half of the frame and the second painter's image in the right
; half of the frame. Similary, below takes two painters and produces a compound painter that draws
; the first painter's image below the second painter's image. Some operations transform a single
; painter to produce a new painter. For example, flip-vert takes a painter and produces a painter
; that draws its image upside down, and flip-horiz produces a painter that draws the original painter's
; image left-to-right reversed.

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(display "Wave 2") (newline)
(define target-2 (make-bitmap 100 100))
(define DC-2 (new bitmap-dc% [bitmap target-2]))
(define frame-2 (make-frame (make-vect 0 0) (make-vect 50.0 0.0) (make-vect 0.0 50.0) DC-2))
(make-object image-snip% target-2)
(wave2 frame-2)

(display "Wave 4") (newline)
(define target-3 (make-bitmap 100 100))
(define DC-3 (new bitmap-dc% [bitmap target-3]))
(define frame-3 (make-frame (make-vect 0 0) (make-vect 50.0 0.0) (make-vect 0.0 50.0) DC-3))
(make-object image-snip% target-3)
(wave4 frame-3)

; In building up a complex image in this manner we are exploiting the fact that painters are closed
; under the language's means of combination. The beside or below of two painters is itself a painter; 
; therefore, we can use it as an element in making more complex painters. As with building up list 
; structure using cons, the closure of our data under the means of combination is crucial to the 
; ability to create complex structures while using only a few operations.

; Once we can combine painters, we would like to be able to abstract typical patterns of combining
; painters. We will implement the painter operations as Scheme procedures. This means that we don't 
; need a special abstraction mechanism in the picture language: Since the means of combination are 
; ordinary Scheme procedures, we automatically have the capability to do anything with painter
; operations that we can do with procedures. For example, we can abstract the pattern in wave4 as

(define (flipped-pairs1 painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(display "flipped-pairs: Should look the same as Wave 4") (newline)
(define target-4 (make-bitmap 100 100))
(define DC-4 (new bitmap-dc% [bitmap target-4]))
(define frame-4 (make-frame (make-vect 0 0) (make-vect 50.0 0.0) (make-vect 0.0 50.0) DC-4))
(make-object image-snip% target-4)
((flipped-pairs wave) frame-4)

; and define wave4 as an instance of this pattern

; (define wave4 (flipped-pairs wave))

; We can also define recursive operations. Here's one that makes painters split and branch towards 
; the right.

(define (right-split1 painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(display "right-split") (newline)
(define target-5 (make-bitmap 100 100))
(define DC-5 (new bitmap-dc% [bitmap target-5]))
(define frame-5 (make-frame (make-vect 0 0) (make-vect 100.0 0.0) (make-vect 0.0 100.0) DC-5))
(make-object image-snip% target-5)
((right-split wave 4) frame-5)

(display "up-split") (newline)
(define target-6 (make-bitmap 100 100))
(define DC-6 (new bitmap-dc% [bitmap target-6]))
(define frame-6 (make-frame (make-vect 0 0) (make-vect 100.0 0.0) (make-vect 0.0 100.0) DC-6))
(make-object image-snip% target-6)
((up-split wave 4) frame-6)

; We can produce balanced patterns by branching upwards as well as towards the right.

(define (corner-split1 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(display "corner-split") (newline)
(define target-7 (make-bitmap 100 100))
(define DC-7 (new bitmap-dc% [bitmap target-7]))
(define frame-7 (make-frame (make-vect 0 0) (make-vect 100.0 0.0) (make-vect 0.0 100.0) DC-7))
(make-object image-snip% target-7)
((corner-split wave 4) frame-7)

; By placing four copies of a corner-split appropriately, we obtain a pattern called square-limit,
; whose application to wave and rogers is shown in Figure 2.9:

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
        (let ((half (beside (flip-horiz quarter) quarter)))
          (below (flip-vert half) half))))

(display "square-limit") (newline)
(define target-8 (make-bitmap 200 200))
(define DC-8 (new bitmap-dc% [bitmap target-8]))
(define frame-8 (make-frame (make-vect 0 0) (make-vect 200.0 0.0) (make-vect 0.0 200.0) DC-8))
(make-object image-snip% target-8)
((square-limit wave 4) frame-8)

; Higher-order operations
; In addition to abstracting patterns of combining painters, we can work at a higher level, abstracting
; patterns of combining painter operations. That is, we can view the painter operations as elements to
; manipulate and can write means of combination for these elements-procedures that take painter operations 
; as arguments and create new painter operations.

; For example, flipped-pairs and square-limit each arrange four copies of a painter's image in a square
; pattern; they differ only in how they orient the copies. One way to abstract this pattern of painter 
; combination is with the following procedure, which takes four one-argument painter operations and 
; produces a painter operation that transforms a given painter with those four operations and arranges
; the results in a square. tl,tr,bl and br are the transformations to apply to the top left copy, the 
; top right copy, the bottom left copy, and the bottom right copy respectively.

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

; Then flipped-pairs can be defined in terms of square-of-four as follows:

(define (flipped-pairs2 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

; and square-limit can be expressed as

(define (square-limit2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


; Frames
; Before we can show how to implement painters and their means of combination, we must first 
; consider frames. A frame can be described by three vectors - an origin vector and two edge
; vectors. The origin vector specifies the offset of the frame's origin from some absolute
; origin in the plane, and the edge vectors specify the offsets of the frame's corners from 
; its origin. If the edges are perpendicular, the frame will be rectangular. Otherwise the frame
; will be a more general parallelogram.

; In accordance with data abstraction we need not be specific yet about how frames are represented,
; other than to say that there is a constructor make-frame, which takes three vectors and produces
; a frame, and three corresponding selectors origin-frame, edge1-frame, and edge2-frame.

; We will use coordinates in the unit square (0 <= x, y <= 1) to specify images. With each frame,
; we associate a frame coordinate map, which will be used to shift and scale images to fit the 
; frame. The map transforms the unit square into the frame by mapping the vector v = (x,y) to the
; vector sum

;     Origin(Frame) + (x * Edge1(Frame)) + (y * Edge2(Frame))

; For example, (0,0) is mapped to the origin of the frame, (1,1) to the vertex diagonally opposite
; the origin, and (0.5,0.5) to the center of the frame. We can create a frame's coordinate map
; with the following procedure.

(define (frame-coord-map1 frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

; Observe that applying frame-coord-map to a frame returns a procedure that, given a vector, 
; returns a vector. If the argument vector is in the unit square, the result vector will be in 
; the frame. For example,

(define a-frame (make-frame (make-vect 0 0) (make-vect 200.0 0.0) (make-vect 0.0 200.0) DC-8))
(display "((frame-coord-map a-frame) (make-vect 0 0))  ")
((frame-coord-map a-frame) (make-vect 0 0))

(display " returns the same vector as ")
; returns the same vector as 

(newline) (display "(origin-frame a-frame)")
(origin-frame a-frame)
(newline)

; Painters
; A painter is represented as a procedure that, given a frame as argument, draws a particular
; image shifted and scaled to fit the frame. That is to say, if p is a painter and f is a frame, then
; we produces p's image in f by calling p with f as argument.

; The details of how primitive painters are implemented depend on the particular characteristics
; of the graphics system and the type of image to be drawn. For instance, suppose we have a procedure
; draw-line that draws a lin on the screen between two specified points. Then we can create painters
; for line drawings, such as the wave painter in Figure 2.10, from lists of line segements as
; follows:

#| ; This is commented out so that we don't have to provide a draw-line procedure
(define (segments->painter1 segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list )))
|#

; The segments are given using coordinates with respect to the unit square. For each segment in 
; the list, the painter transforms the segment end-points with the frame coordinate map and draws
; a line between the transformed points.

; Representing painters as procedures erects a powerful abstraction barrier in the picture language.
; We can create and intermix all sorts of primitive painters, based on a variety of graphics
; capabilities. The details of their implementation do not matter. Any procedure can serve as a
; painter, provided that it takes a frame as argument and draws something scaled to fit the frame.


; Transforming and combining painters
; An operation on painters (such as flip-vert or beside) works by creating a painter that invokes
; the original painters with respect to frames derived from the argument frame. Thus, for example
; flip-vert doesn't have to know how a painter works in order to flip it - it just has to know how
; to turn a frame upside down: The flipped painter just uses the original painter, but in the 
; inverted frame.

; Painter operations are based on the procedure transform-painter, which takes as arguments a
; painter and information on how to transform a frame and produces a new painter. The transformed
; painter, when called on a frame, transforms the frame and calls the original painter on the
; transformed frame. The arguments to transform-painter are points (represented as vectors) that 
; specify the corners of the new frame: When mapped into the frame, the first point specifies
; the new frame's origin and the other two specify the ends of its edge vectors. Thus, arguments
; within the unit square specify a frame contained within the original frame.

(define (transform-painter1 painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

; Here's how to flip painter images vertically

(define (flip-vert1 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

; Using transform-painter, we can easily define new transformations. For example, we can define
; a painter that shrinks its image to the upper-right quarter of the frame it is given:

(define (shrink-to-upper-right1 painter)
  (transform-painter
   painter (make-vect 0.5 0.5)
   (make-vect 1.0 0.5) (make-vect 0.5 1.0)))

; Other transformations rotate images counterclockwise by 90 degrees

(define (rotate901 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

; or squash imapes towards the center of the frame:

(define (squash-inwards1 painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

; Frame transformation is als the key to defining means of combining two or more painters.
; The beside procedure, for example, takes two painters, transforms them to paint in the left 
; and right halves of an argument frame respectively, and prduces a new, compoint painter.

; When the compound painter is given a frame, it calls the first transformed painter to paint 
; in the left half of the frame and calls the second transformed painter to paint in the right 
; half of the frame:

(define (beside1 painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left (transform-painter painter1 (make-vect 0.0 0.0) 
                                         split-point (make-vect 0.0 1.0)))
          (paint-right (transform-painter painter2 (make-vect 1.0 0.0) 
                                          split-point (make-vect 1.0 0.0) (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

; Observe how the painter data abstraction, and in particular the representation of painters as
; procedures, makes beside easy to implement. The beside procedure need not know anything about
; the details of the component painters other than that each painter will draw something in 
; its designated frame.

; Levels of language for robust design is continued on in the 2.2 file

