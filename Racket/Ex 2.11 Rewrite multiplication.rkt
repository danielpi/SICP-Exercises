#lang racket

; Exercise 2.10
; By testing the signs of the endpoints of the intervals it is possible to break 
; mul-interval into nine cases, only one of which requires more than two multiplications.

; Rewrite this procedure to take advantage of this fact.

(define (negative? x) (< x 0))
(define (positive? x) (> x 0))
(define (make-interval lower upper)
  (cons (min lower upper) (max lower upper)))
(define (lower-bound interval)
  (min (car interval) (cdr interval)))
(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))
(define (mul-interval-old x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ly (lower-bound y))
        (ux (upper-bound x))
        (uy (upper-bound y)))
    (cond ((and (negative? lx) (negative? ly) (negative? ux) (negative? uy)) (make-interval (* lx ly)
                                                                                             (* ux uy)))
           ((and (positive? lx) (positive? ly) (positive? ux) (positive? uy)) (make-interval (* lx ly) 
                                                                                             (* ux uy)))
           ((and (negative? lx) (negative? ly) (positive? ux) (positive? uy)) (make-interval (min (* lx uy) (* ux ly))
                                                                                             (* ux uy)))
           ((and (negative? lx) (positive? ly) (positive? ux) (positive? uy)) (make-interval (* lx uy) 
                                                                                             (* ux uy)))
           ((and (positive? lx) (negative? ly) (positive? ux) (positive? uy)) (make-interval (* ux ly) 
                                                                                             (* ux uy)))
           ((and (negative? lx) (negative? ly) (positive? ux) (negative? uy)) (make-interval (* ux ly) 
                                                                                             (* lx ly)))
           ((and (negative? lx) (negative? ly) (negative? ux) (positive? uy)) (make-interval (* lx uy) 
                                                                                             (* lx ly)))
           ((and (negative? lx) (positive? ly) (negative? ux) (positive? uy)) (make-interval (* ux ly) 
                                                                                             (* lx uy)))
           ((and (positive? lx) (negative? ly) (positive? ux) (negative? uy)) (make-interval (* lx uy) 
                                                                                             (* ux ly)))
           (else (error "Didn't handle this case")))))
            
           
           
           
(define (spans-zero interval)
  (if (> (upper-bound interval) 0)
      (< (lower-bound interval) 0)
      #f))
(define (div-interval x y)
  (if (or (spans-zero x) (spans-zero y))
      (error "Interval division spans zero")
      (mul-interval-old x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define r1 (make-interval -1 3))
(define r2 (make-interval 2 5))
(define r3 (make-interval -4 -2))
(define r4 (make-interval 0 1))
(define r5 (make-interval -4 0))


; Case 1
; Both intervals are positive
; (make-interval (* (lower-bound x) (lower-bound y))
;                (* (upper-bound x) (upper-bnound y)))
(display "Case 1") (newline)
(mul-interval-old (make-interval 1 3) (make-interval 1 3))
(mul-interval (make-interval 1 3) (make-interval 1 3))

; Case 2
; Both intervals are negative
; (make-interval (* (lower-bound x) (lower-bound y))
;                (* (upper-bound x) (upper-bnound y)))
(display "Case 2") (newline)
(mul-interval-old (make-interval -1 -3) (make-interval -1 -3))
(mul-interval (make-interval -1 -3) (make-interval -1 -3))

; Case 3
; Both intervals span 0
; (let ((p2 (* (lower-bound x) (upper-bound y)))
;       (p3 (* (upper-bound x) (lower-bound y)))
;       (p4 (* (upper-bound x) (upper-bound y))))
;   (make-interval (min p2 p3)
;                  p4)))
(display "Case 3") (newline)
(mul-interval-old (make-interval -1 3) (make-interval -2 3))
(mul-interval (make-interval -1 3) (make-interval -2 3))

; Case 4
; One interval spans 0 the other is positive
(display "Case 4") (newline)
(mul-interval-old (make-interval -1 3) (make-interval 2 3))
(mul-interval (make-interval -1 3) (make-interval 2 3))

; Case 5
; One interval is positive and the other spans 0
(display "Case 5") (newline)
(mul-interval-old (make-interval 1 3) (make-interval -2 3))
(mul-interval (make-interval 1 3) (make-interval -2 3))

; Case 6
; One interval spans 0 the other is negative
(display "Case 6") (newline)
(mul-interval-old (make-interval -1 3) (make-interval -2 -3))
(mul-interval (make-interval -1 3) (make-interval -2 -3))

; Case 7
; One interval is negative and the other spans 0
(display "Case 7") (newline)
(mul-interval-old (make-interval -1 -3) (make-interval -2 3))
(mul-interval (make-interval -1 -3) (make-interval -2 3))

; Case 8
; One interval is negative and the other is positive
(display "Case 8") (newline)
(mul-interval-old (make-interval -1 -3) (make-interval 2 3))
(mul-interval (make-interval -1 -3) (make-interval 2 3))

; Case 9
; One interval is positive and the other is negative
(display "Case 9") (newline)
(mul-interval-old (make-interval 1 3) (make-interval -2 -3))
(mul-interval (make-interval 1 3) (make-interval -2 -3))