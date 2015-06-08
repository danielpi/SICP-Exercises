#lang racket

; Exercise 2.45
; right-split and up-split can be expressed as instances of a general splitting operation. Define
; a procedure split with the property that evaluating
;
; (define right-split (split beside below))
; (define up-split (split below beside))
;
; produces procedures right-split and up-split with the same behaviours as the ones already defined

(define (right-split-prev painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split-prev painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split-prev painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split-prev painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (split orig-placer split-placer)
  (define (split-internal painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-iter painter (- n 1))))
          (orig-placer painter (split-placer smaller smaller)))))
  split-internal)

(define right-split (split beside below))
(define up-split (split below beside))