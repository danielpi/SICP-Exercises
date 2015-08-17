#lang racket

; Exercise 2.32
; We can represent a set as a list of distinct elements, and we can represent the set of all
; subsets of the set as a list of lists. For example, if the set is (1 2 3), then the set of
; all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following definintion
; of a procedure that generates the set of subsets of a set and give a clear explanation of
; why it works:

(define (reverse list)
  (define (reverse-iter list reverse-list)
    (if (null? list)
        reverse-list
        (reverse-iter (cdr list) (cons (car list) reverse-list))))
  (reverse-iter list '()))

(define (contains? A b)
  (cond ((null? A) #f)
        ((= (car A) b) #t)
        (else (or (contains? (cdr A) b)))))
;(contains? (list 1 2 3) 1) ; #t
;(contains? (list 1 2 3) 3) ; #t
;(contains? (list 1 2 3) 5) ; #f

; Build up the output by grabbing each value from A that is not contained in B and appending
; it to our output list.
(define (relative-complement A B)
  (define (iter AA BB output)
    (if (null? AA)
        output
        (if (contains? BB (car AA))
            (iter (cdr AA) BB output)
            (iter (cdr AA) BB (append (list (car AA)) output)))))
  (reverse (iter A B '())))
  
; (relative-complement (list 2 3) (list 3)) ; (2)
; (relative-complement (list 1 2 3) (list 2)) ; (1 3)


(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset) (relative-complement s subset)) (reverse rest))))))

(subsets (list 1 2 3))

; (subset (1 2 3))
; (append (subsets (2 3)) (map (??) (subsets (2 3))))))
; (append ((append (subsets (3)) (map (??) (subsets (3))))))) (map (??) (append (subsets (3)) (map (??) (subsets (3))))))))))
; (append ((append (append (subsets ()) (map (??) (subsets ()))))) (map (??) (append (subsets ()) (map (??) (subsets ())))))))))) (map (??) (append (append (subsets ()) (map (??) (subsets ()))))) (map (??) (append (subsets ()) (map (??) (subsets ())))))))))))))
; (append ((append (append () (map (??) ())))) (map (??) (append () (map (??) ()))))))))) (map (??) (append (append () (map (??) ())))) (map (??) (append () (map (??) ()))))))))))))
; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))


(subsets (list 2 3))

; (subset (2 3))
; (s (2 3)) (rest (subset (3)) (append rest (map (??) rest))
; (s (2 3)) (append (() (3)) (map (??) (() (3))))
; (() (3) (2) (2 3))

(subsets (list 3))
; (s (3)) (rest (subsets '()) (append rest (map (??) rest))
; (() (3))


(subsets '())
; (s '()) (list '()) 
; (())
