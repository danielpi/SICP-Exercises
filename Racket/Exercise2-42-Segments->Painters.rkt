#lang racket

; Exercise 2.49
; Use segments->painter to define the following primitive painters:
; a. The painter that draws the outline of the designated frame.
; b. The painter that draws an "X" by connecting opposite corners of the frame.
; c. The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
; d. The wave painter.

(require pict)

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
  (make-vect (- (xcor-vect vec2) (xcor-vect vec1))
             (- (ycor-vect vec2) (ycor-vect vec1))))
(define (scale-vect scale vect)
  (make-vect (* scale (xcor-vect vect))
             (* scale (ycor-vect vect))))

(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))


(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line
;        ((frame-coord-map frame)
;         (start-segment segment))
;        ((frame-coord-map frame)
;         (end-segment segment))))
;     segment-list )))
(define (segments->painter segment-list)
  (lambda (frame)
    (dc (lambda (dc dx dy)
          (for-each
           (lambda (segment)
             (send dc draw-line
                   (xcor-vect ((frame-coord-map frame) (start-segment segment)))
                   (ycor-vect ((frame-coord-map frame) (start-segment segment)))
                   (xcor-vect ((frame-coord-map frame) (end-segment segment)))
                   (ycor-vect ((frame-coord-map frame) (end-segment segment)))))
           segment-list)) 
        150 150)))

;((segments->painter (list (make-segment (make-vect 0 0) (make-vect 20 20))
;                          (make-segment (make-vect 20 0) (make-vect 0 20)))) 
; (make-frame (make-vect 0 0) (make-vect 10 0) (make-vect 0 10)))

; a. The painter that draws the outline of the designated frame.
(define (frame-outline frame)
  (let ((p1 (origin-frame frame))
        (p2 (add-vect (origin-frame frame) (edge1-frame frame)))
        (p3 (add-vect (add-vect (origin-frame frame) (edge1-frame frame)) (edge2-frame frame)))
        (p4 (add-vect (origin-frame frame) (edge2-frame frame))))
    ((segments->painter (list (make-segment p1 p2)
                              (make-segment p2 p3)
                              (make-segment p3 p4)
                              (make-segment p4 p1))) frame)))
(frame-outline  (make-frame (make-vect 0 0) (make-vect 10 0) (make-vect 0 10)))
 
; b. The painter that draws an "X" by connecting opposite corners of the frame.
(define (frame-cross frame)
  (let ((p1 (origin-frame frame))
        (p2 (add-vect (origin-frame frame) (edge1-frame frame)))
        (p3 (add-vect (add-vect (origin-frame frame) (edge1-frame frame)) (edge2-frame frame)))
        (p4 (add-vect (origin-frame frame) (edge2-frame frame))))
    ((segments->painter (list (make-segment p1 p3)
                              (make-segment p2 p4))) frame)))
(frame-cross  (make-frame (make-vect 0 0) (make-vect 10 0) (make-vect 0 10)))
 
; c. The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
(define (midpoint-segment segment)
  (add-vect (start-segment segment)
            (scale-vect 0.5 (end-segment segment))))
(define (frame-diamond frame)
 (let ((p1 (origin-frame frame))
        (p2 (add-vect (origin-frame frame) (edge1-frame frame)))
        (p3 (add-vect (add-vect (origin-frame frame) (edge1-frame frame)) (edge2-frame frame)))
        (p4 (add-vect (origin-frame frame) (edge2-frame frame))))
    (let ((s1 (make-segment p1 p2))
          (s2 (make-segment p2 p3))
          (s3 (make-segment p3 p4))
          (s4 (make-segment p4 p1)))
      ((segments->painter (list s1
                                s2
                                s3
                                s4
                                (make-segment (midpoint-segment s1) (midpoint-segment s2))
                                (make-segment (midpoint-segment s2) (midpoint-segment s3))
                                (make-segment (midpoint-segment s3) (midpoint-segment s4))
                                (make-segment (midpoint-segment s4) (midpoint-segment s1))
                                )) frame))))
(frame-diamond  (make-frame (make-vect 0 0) (make-vect 10 0) (make-vect 0 10)))

(scale-vect 0.5 (make-vect 0 2))
(midpoint-segment (make-segment (make-vect 2 2) (make-vect 0 2)))