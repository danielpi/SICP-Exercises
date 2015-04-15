#lang racket

; Exercise 2.29
; A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod of
; a certain length, from which hangs either a weight or another binary mobile. We can represent a 
; binary mobile using compound data by constructing it from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))

; A branch is constructed from a length (which must be a number) together with a structure, which 
; may be either a number (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))


;     |
; ---------
; 2       |
;       -----
;       1   1

(define a-mobile (make-mobile 
                  (make-branch 3 2)
                  (make-branch 3 (make-mobile 
                                  (make-branch 1 1)
                                  (make-branch 1 1)))))


; a) Write the corresponding selectors left-branch and right-branch, which return the branches
;    of a mobile, and branch-length and branch-structure, which return the components of a branch.

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch)))

a-mobile                                     ; ((3 2) (3 ((1 1) (1 1))))
(left-branch a-mobile)                       ; (3 2)
(branch-length (left-branch a-mobile))       ; 3
(branch-structure (left-branch a-mobile))    ; 2
(right-branch a-mobile)                      ; (3 ((1 1) (1 1)))
(branch-length (right-branch a-mobile))      ; 3
(branch-structure (right-branch a-mobile))   ; ((1 1) (1 1))


; b) Using your selectors, define a procedure total-weight that returns the total weight of 
;    a mobile

(define (total-weight structure)
  (if (pair? structure)
      (let ((left (branch-structure (left-branch structure)))
            (right (branch-structure (right-branch structure))))
        (+ (if (pair? left) (total-weight left) left)
           (if (pair? right) (total-weight right) right)))
      structure))
  
(total-weight a-mobile)                      ; 4


; c) A mobile is said to be balanced if the torque applied by its top-left branch is equal to
;    that applied by its top-right branch (that is, if the length of the left rod multiplied by
;    the weight hanging from that rod is equal to the corresponding product for the right side) and
;    if each of the submobiles hanging off its branches is balanced. Design a predicate that tests
;    whether a binary mobile is balanced.

(define (balanced? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (= (* (branch-length left) (total-weight (branch-structure left)))
            (* (branch-length right) (total-weight (branch-structure right))))
         (if (pair? (branch-structure left)) (balanced? (branch-structure left)) #t)
         (if (pair? (branch-structure right)) (balanced? (branch-structure right)) #t))))

(balanced? a-mobile)                         ; #t


; d) Suppose we change the representation of mobiles so that the constructors are
(define (make-mobile2 left right) (cons left right))
(define (make-branch2 length structure) (cons length structure))
;    How much do you need to change your programs to convert to the new representation?

(define (left-branch2 mobile) (car mobile))
(define (right-branch2 mobile) (cdr mobile))
(define (branch-length2 branch) (car branch))
(define (branch-structure2 branch) (cdr branch))


(define b-mobile (make-mobile2 
                  (make-branch2 3 2)
                  (make-branch2 3 (make-mobile2 
                                  (make-branch2 1 1)
                                  (make-branch2 1 1)))))

b-mobile                                      ; ((3 . 2) 3 (1 . 1) 1 . 1)
(left-branch2 b-mobile)                       ; (3 . 2)
(branch-length2 (left-branch2 b-mobile))      ; 3
(branch-structure2 (left-branch2 b-mobile))   ; 2
(right-branch2 b-mobile)                      ; (3 (1 . 1) 1 . 1)
(branch-length2 (right-branch2 b-mobile))     ; 3
(branch-structure2 (right-branch2 b-mobile))  ; ((1 . 1) 1 . 1)
