#lang racket

; Exercise 1.45
; We saw in section 1.3.3 that attempting to compute square roots by naively finding a 
; fixed-point of y -> x/y does not converge, and that this can be fixed by average damping.
; The same method works for finding cube roots as fixed points of the average damped 
; y -> x/y^2. Unfortunately, the process does not work for fourth roots -- a single average
; damp is not enough to make a fixed-point search for y -> x/y^3 converge. On the other hand, if
; we average damp twice the fixed-point search does converge. 

; Do some experiments to determine how many average damps are required to compute nth roots
; as a fixed point search based upon repeated average damping of y -> x/y^(n-1).

; Use this to implement a simple procedure for computing nth roots using fixed-point, average-damp
; and the repeated procedure of exercise 1.43.


(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average a b) (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter g step)
    (if (= step 1)
        g
        (iter (compose f g) (- step 1))))
  (iter f n))
 

(define (dampings n)
  (floor (/ (log n) (log 2))))

(define (nth-root x n)
  (fixed-point ((repeated average-damp (dampings n)) (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

                ; Order | Damping
(nth-root 2 2)  ;     2 |       1
(nth-root 2 3)  ;     3 |       1
(nth-root 2 4)  ;     4 |       2
(nth-root 2 5)  ;     5 |       2
(nth-root 2 6)  ;     6 |       2
(nth-root 2 7)  ;     7 |       2
(nth-root 2 8)  ;     8 |       3
(nth-root 2 9)  ;     9 |       3
(nth-root 2 10) ;    10 |       3
(nth-root 2 11) ;    11 |       3
(nth-root 2 12) ;    12 |       3
(nth-root 2 13) ;    13 |       3
(nth-root 2 14) ;    14 |       3
(nth-root 2 15) ;    15 |       3
(nth-root 2 16) ;    16 |       4
(nth-root 2 160);   160 |       7






