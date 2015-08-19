#lang racket

; Exercise 2.75
; Implement the constructor make-from-mag-ang in message-passing style. This procedure
; should be analogous to the make-from-real-imag-procedure given above.

(define (square x) (* x x))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          (else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (real-part i) (apply-generic 'real-part i))
(define (imag-part i) (apply-generic 'imag-part i))
(define (magnitude i) (apply-generic 'magnitude i))
(define (angle i) (apply-generic 'angle i))

(define a (make-from-real-imag 3 4))
(define b (make-from-mag-ang 1 3.14))

(display "Operations on a number in rectangular form")(newline)
(display "Real      ")(real-part a)
(display "Imaginary ")(imag-part a)
(display "Magnitude ")(magnitude a)
(display "Angle     ")(angle a)
(newline)
(display "Operations in a number in polar form")(newline)
(display "Real      ")(real-part b)
(display "Imaginary ")(imag-part b)
(display "Magnitude ")(magnitude b)
(display "Angle     ")(angle b)
  