#lang racket

; Exercise 1.30
; The sum procedure generates a linear recursion. The procedure can be rewritten
; so that the sum is performed iteratively. Show how to do this by filling in the
; missing expressions

; (define (sum term a next b)
;   (define (iter a result)
;     (if <??>
;         <??>
;         (iter <??> <??>)))
;   (iter <??> <??>))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

; (define (sum term a next b)
;   (if (> a b)
;       0
;       (+ (term a)
;         (sum term (next a) next b))))

(define (cube x) (* x x x))
(define (identity x) x)
(define (inc x) (+ x 1))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* h k))))
  (define (simpson-term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((odd? k) 4)
             (else 2))
       (yk k)))
  (* (/ h 3) (sum simpson-term 0 inc n)))

(integral cube 0 1.2 100)
(integral cube 0 1 1000)