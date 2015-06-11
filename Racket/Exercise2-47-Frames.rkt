#lang racket

; Exercise 2.47
; Here are two possible constructors for frames:

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

; For each constructor supply the appropriate selectors to produce an implementation 
; for frames.

(define (origin1 frame)
  (car frame))
(define (edge11 frame)
  (car (cdr frame)))
(define (edge21 frame)
  (car (cdr (cdr frame))))

(define (origin2 frame)
  (car frame))
(define (edge12 frame)
  (car (cdr frame)))
(define (edge22 frame)
  (cdr (cdr frame)))



(define (make-vect x y)
  (cons x y))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))

(define a (make-vect 3 4))
(define b (make-vect 2 1))
(define c (make-vect 6 8))

(define d (make-frame1 a b c))
(origin1 d)
(edge11 d)
(edge21 d)

(define e (make-frame2 a b c))
(origin1 e)
(edge11 e)
(edge22 e)