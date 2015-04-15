#lang racket

; Exercise 2.25
; Give combinations of cars and cdrs that will pick 7 from each of the following lists

(define a (list 1 3 (list 5 7) 9))
; (1 3 (5 7) 9)
;       ^
;     /   \
;    1  (3 (5 7) 9)
;            ^
;          /   \
;         3  ((5 7) 9)
;                ^
;              /   \
;           (5 7)   9
;             ^
;           /   \
;          5     7
(car (cdr (car (cdr (cdr a)))))


(define b (list (list 7)))
; ((7))
;   |
;  (7)
;   |
;   7
(car (car b))


(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
; (1 (2 (3 (4 (5 (6 7))))))
;           ^
;         /   \
;        1  (2 (3 (4 (5 (6 7)))))
;               ^
;             /   \
;            2  (3 (4 (5 (6 7))))
;                   ^
;                 /   \
;                3   (4 (5 (6 7)))
;                       ^
;                     /   \
;                    4     (5 (6 7))
;                           ^
;                         /   \
;                        5     (6 7)
;                               ^
;                             /   \
;                            6     7
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))
