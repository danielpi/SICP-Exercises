#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
       '()
       (cons (accumulate op init (map car seqs))
             (accumulate-n op init (map cdr seqs)))))

; Exercise 2.37
; Suppose we represent vectors v = (v1) as sequences of numbers, and matrices m = (mij) as sequences
; of vectors (the rows of the matrix). For example, the matrix

; (( 1 2 3 4 )
;  ( 4 5 6 6 )
;  ( 6 7 8 9 ))

; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this representation, we can 
; use sequence operations to concisely express the basic matrix and vector operations. These
; operations (which are described in any book on matrix algebra) are the following:

; (dot-product v w) 
; (matrix-*-vector m v) 
; (matrix-*-matrix m n)
; (transpose m)

; We can define the dot product as

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; Fill in the missing expressions in the following procedures for computing the other matrix 
; operations. (The procedure accumulate-n is defined in Exercise 2.36)

(define (matrix-*-vector m v)
  (map <> m))

(define (transpose mat)
  (accumulate-n <> <> mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map <> m)))