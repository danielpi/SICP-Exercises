#lang racket

; Exercise 2.50
; Define the transformation flip-horiz, which flips painters horizontally, and transformations
; that roatate painters counterclockwise by 180 degrees and 270 degrees.

(require racket/draw)
(require racket/gui)

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

(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

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


(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)
                  (dc frame)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

(define (shrink-to-upper-right painter)
  (transform-painter painter 
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5) 
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.35 0.35)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))


(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left (transform-painter painter1 (make-vect 0.0 0.0) split-point (make-vect 0.0 1.0)))
          (paint-right (transform-painter painter2 split-point (make-vect 1.0 0.0) (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (do-nothing painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 1.0)))

(define target (make-bitmap 100 100))
(define aDC (new bitmap-dc% [bitmap target]))
(define aFrame (make-frame (make-vect 00 0) (make-vect 50.0 0.0) (make-vect 0.0 50.0) aDC))
(frame-diamond  aFrame)
(make-object image-snip% target)


;(frame-cross  aFrame)
;(frame-diamond  aFrame)
;((flip-vert frame-diamond) aFrame)
;((shrink-to-upper-right frame-outline) aFrame)
;((do-nothing frame-outline) aFrame)
;((rotate90 frame-cross) aFrame)
;((squash-inwards frame-cross) aFrame)

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)   ; new origin
                     (make-vect 0.0 0.0)   ; new end of edge1
                     (make-vect 1.0 1.0))) ; new end of edge2

(define targetB (make-bitmap 100 100))
(define bDC (new bitmap-dc% [bitmap targetB]))
(define bFrame (make-frame (make-vect 00 0) (make-vect 50.0 0.0) (make-vect 0.0 50.0) bDC))
((flip-horiz (beside frame-cross frame-diamond)) bFrame)
(make-object image-snip% targetB)


(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define targetC (make-bitmap 100 100))
(define cDC (new bitmap-dc% [bitmap targetC]))
(define cFrame (make-frame (make-vect 00 0) (make-vect 50.0 0.0) (make-vect 0.0 50.0) cDC))
((rotate180 (beside frame-cross frame-diamond)) cFrame)
(make-object image-snip% targetC)


(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define targetD (make-bitmap 100 100))
(define dDC (new bitmap-dc% [bitmap targetD]))
(define dFrame (make-frame (make-vect 00 0) (make-vect 50.0 0.0) (make-vect 0.0 50.0) dDC))
((rotate270 (beside frame-cross frame-diamond)) dFrame)
(make-object image-snip% targetD)

