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
             (let ((start ((frame-coord-map frame) (start-segment segment)))
                   (end (add-vect ((frame-coord-map frame) (start-segment segment))
                                  ((frame-coord-map frame) (end-segment segment)))))
                   (send dc draw-line
                          (xcor-vect start)
                          (ycor-vect start)
                          (xcor-vect end)
                          (ycor-vect end))))
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
    ((segments->painter (list (make-segment p1 (edge1-frame frame))
                              (make-segment p1 (edge2-frame frame))
                              (make-segment p2 (edge2-frame frame))
                              (make-segment p4 (edge1-frame frame)))) frame)))
(frame-outline  (make-frame (make-vect 0 0) (make-vect 10 0) (make-vect 0 10)))
 
; b. The painter that draws an "X" by connecting opposite corners of the frame.
(define (frame-cross frame)
  (let ((p1 (origin-frame frame))
        (p2 (add-vect (origin-frame frame) (edge1-frame frame)))
        (p3 (add-vect (add-vect (origin-frame frame) (edge1-frame frame)) (edge2-frame frame)))
        (p4 (add-vect (origin-frame frame) (edge2-frame frame)))
        (p1-p3 (add-vect (edge1-frame frame) (edge2-frame frame)))
        (p2-p4 (sub-vect (edge1-frame frame) (edge2-frame frame))))
    ((segments->painter (list (make-segment p1 p1-p3)
                              (make-segment p2 p2-p4))) frame)))
(frame-cross  (make-frame (make-vect 0 0) (make-vect 10 0) (make-vect 0 10)))
 
; c. The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
(define (midpoint-segment segment)
  (add-vect (start-segment segment)
            (scale-vect 0.5 (end-segment segment))))
(define (point-to-point a b)
  (sub-vect b a))
(define (frame-diamond frame)
 (let ((m1 (midpoint-segment (make-segment (origin-frame frame) (edge1-frame frame))))
       (m2 (midpoint-segment (make-segment (add-vect (origin-frame frame) (edge1-frame frame)) (edge2-frame frame))))
       (m3 (midpoint-segment (make-segment (add-vect (origin-frame frame) (edge2-frame frame)) (edge1-frame frame))))
       (m4 (midpoint-segment (make-segment (origin-frame frame) (edge2-frame frame)))))
   ((segments->painter (list (make-segment m1 (point-to-point m2 m1))
                             (make-segment m2 (point-to-point m3 m2))
                             (make-segment m3 (point-to-point m4 m3))
                             (make-segment m4 (point-to-point m1 m4)))) frame)))
                        
(frame-diamond  (make-frame (make-vect 0 0) (make-vect 10 0) (make-vect 0 10)))


; d. The wave painter.
