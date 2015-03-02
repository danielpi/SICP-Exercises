#lang racket

; Exercise 1.37
; An infinite continued fraction is an expression of the form
;
;            N1
; f = ------------------
;     D1 +      N2
;          -------------
;          D2 +    N3
;               --------
;               D3 + ...
;
; As an example, the inifinite continued fraction expansion with the Ni and Di all
; equal to 1 produces 1 / Golden Ratio.
;
; One way to approximate an infinite continued fraction is to truncate the expansion
; after a given number of terms. Such a truncation -- a so-called k-term finitie continued
; fraction -- has the form
;
;            N1
; f = ------------------
;     D1 +      N2
;          -------------
;          D2 +    NK
;               --------
;                  DK
;
; Suppose that n and d are procedures of one argument (the term i) that return the Ni and
; Di of the terms of the continued fraction. Define a procedure cont-frac such that evaluating
; (cont-frac n d k) computes the value of the k-term finite continued fraction. Check your
; procedure by approximating 1 / Golden ratio

(define (cont-frac Ni Di k)
  (if (= k 1)
      (/ (Ni k) (Di k))
      (/ (Ni k) (+ (Di k) (cont-frac Ni Di (- k 1))))))
(define (golden-ratio)
  (/ 1 (cont-frac (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  13)))
(golden-ratio)

; How large must k be in order to get an approximation that is accurate to 4 decimal places?
; k needs to be 13 or higher


; b) 
; Write an iterative version of cont-frac

(define (cont-frac-iterative Ni Di k)
  (define (iter k result)
    (if (= k 0)
        result
        (iter (- k 1) (/ (Ni k) (+ (Di k) result)))))
  (iter k 0))
(define (golden-ratio2)
  (/ 1 (cont-frac-iterative (lambda (i) 1.0)
                            (lambda (i) 1.0)
                            100)))
(golden-ratio2)

  


  