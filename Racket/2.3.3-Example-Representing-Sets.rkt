#lang racket

; 2.3.3 Example: Representing Sets

; In the previous examples we built representations for two kinds of compound data objects: 
; rational numbers and algebraic expressions. In one of these examples we had the choise of 
; simplifying (reducing) the expressions at either construction time or selection time, but
; other than that the choice of a representation for these structures in terms of lists was
; straightforward. When we turn to the representation of sets, the choice of a representation
; is not so obvious. Indeed, there are a number of possible representations, and the differ
; significantly from one another in several ways.

; Informally, a set is simply a collection of distinct objects. To give a more precise definition
; we can employ the method of data abstraction. That is, we define "set" by specifying the 
; operations that are to be used on sets. These are union-set, intersection-set, element-of-set?
; and adjoin-set. element-of-set? is a predicate that determines whether a given element is
; a member of a set. adjoin-set takes an object and a set as arguments and returns a set that 
; contains the elements of the original set and also the adjoined element. union-set computes
; the union of two sets, which is the set containing each element that appears in either argument.
; intersection-set computes the intersection of two sets, which is the set containing only
; elements that appear in both arguments. From the viewpoint of data abstraction, we are free
; to design any representation that implements these operations in a way consistent with the
; interpretations given above.

; Sets as unordered lists
; One way to represent a set is as a list of its elements in which no element appears more
; than once. The empty set is represented by the empty list. In this representation, 
; element-of-set? is similar to the procedure memq of Section 2.3.1. It uses equal? instead 
; of eq? so that the set elements need not be symbols

(define (element-of-set?1 x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set?1 x (cdr set)))))

(display "element-of-set?1")(newline)
(define set1 (list 1 2 5 8 9))
(element-of-set?1 2 set1)
(element-of-set?1 3 set1)

; Using this, we can write adjoin-set. If the object to be adjoined is already in the set, 
; we just return the set. Otherwise, we use cons to add the object to the list that represents
; the set:

(define (adjoin-set1 x set)
  (if (element-of-set?1 x set)
      set
      (cons x set)))

(display "adjoin-set1")(newline)
(adjoin-set1 3 set1)
(adjoin-set1 2 set1)

; For interection-set we can use a recursive strategy. If we know how to form the intersection 
; of set2 and the cdr of set1, we only need to decide whether to include the car of set1 in 
; this. But this depends on whether (car set1) is also in set2. Here is the resulting procedure:

(define (intersection-set1 set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set?1 (car set1) set2)
         (cons (car set1) (intersection-set1 (cdr set1) set2)))
        (else (intersection-set1 (cdr set1) set2))))

(display "intersection-set1")(newline)
(intersection-set1 set1 (list 2 3 4 5 6))

; In designing a representation, one of the issues we should be concerned with is efficiency.
; Consider the number of steps required by our set operations. Since they all use element-of-set?
; the speed of this operation as a major impact on the efficiency of the set implementation as
; a whole. Now, in order to check whether an object is a member of a set, element-of-set? may
; have to scan the entire set. (In the worst case, the object turns out not to be in the set).
; Hence, if the set has n elements, element-of-set? might take up to n steps. Thus, the number
; of steps required grows as O(n). The number of steps required by adjoin-set, which uses this
; operation, also grows as O(n). For intersection-set, which does an element-of-set? check for
; each element of set1, the number of steps required grows as the product of the sizes of the
; sets involved, or O(n^2) for two sets of size n. The same will be true of union-set.


; Exercise 2.59
(define (union-set1 set1 set2)
  (if (null? set1)
      set2
      (union-set1 (cdr set1) (adjoin-set1 (car set1) set2)))) 

(display "union-set1")(newline)
(union-set1 set1 (list 2 3 4 5 6 7))


; Exercise 2.60 
