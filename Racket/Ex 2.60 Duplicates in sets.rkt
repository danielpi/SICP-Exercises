#lang racket

; Exercise 2.60
; We specified that a set would be represented as a list with no duplicates. Now 
; suppose we allow duplicates. For instance, the set {1,2,3} could be represented
; as the list (2 3 2 1 3 2 2). Design procedures element-of-set?, adjoin-set,
; union-set, and intersection-set that operate on this representation. How does 
; the efficiency of each compare with the corresponding procedure for the non-
; duplicate representation? Are there applications for which you would use this
; representation in preference to the non-duplicate one?

(define set1 (list 2 3 2 1 3 2 2))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? (car set) x) #t)
        (else (element-of-set? x (cdr set)))))
(display "element-of-set?")(newline)
(element-of-set? 1 set1)
(element-of-set? 2 set1)
(element-of-set? 3 set1)
(element-of-set? 4 set1)
; element-of-set? is on average much worse efficiency wise as there may be many more
; items in the set to search through. It is still O(n) but n relates to all elements
; in the set (including duplicates) and as such can be much larger.


(define (adjoin-set x set)
  (cons x set))
(display "adjoin-set")(newline)
(adjoin-set 3 set1)
(adjoin-set 4 set1)
; adjoin-set is much more efficient because we no longer need to search the set to
; identify if we are about to add a duplicate. We now have a constant time algorithm
; O(1)


(define (union-set set1 set2)
  (append set1 set2))
(display "union-set")(newline)
(union-set (list 1 2 3 4) (list 3 4 5 6))
; As with adjoin set this is much more efficient. No more searching for duplicats
; just bang them together. The efficency is O(append)


(define (intersection-set set1 set2)
  (define (iter s1 s2 result)
    (if (null? s1)
        result
        (if (element-of-set? (car s1) s2)
            (iter (cdr s1) s2 (adjoin-set (car s1) result))
            (iter (cdr s1) s2 result))))
  (iter set1 set2 '()))
(display "intersection-set")(newline)
(intersection-set (list 1 2 3 4) (list 3 4 5 6))
(intersection-set (list 1 2 3 4 2 8 7 2 1 4 0) (list 3 3 3 4 4 5 5 6 6))

(define (intersection-set1 set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set1 (cdr set1) set2)))
        (else (intersection-set1 (cdr set1) set2))))
(intersection-set1 (list 1 2 3 4 2 8 7 2 1 4 0) (list 3 3 3 4 4 5 5 6 6))
; intersection-set is in the same boat as element-of-set? in that it is basically the
; same algorithm but now the size of the sets can be significantly larger. O(n^2)

; You would use this version in applications where you are writting to the set frequently
; but rarely reading from the set.
