#lang racket

; Exercise 2.70
; The following eight-symbol alphabet with associated relative frequencies was designed to
; efficiently encode lyrics of 1950s rock songs. (note that the "symbols" of an "alphabet"
; need not be individual letters.)

;   A    2  GET  2  SHA  3  WAH  1
;   BOOM 1  JOB  2  NA  16  YIP  9

; Use generate-huffman-tree to generate a corresponding Huffman tree, and use encode to
; encode the following message

; Get a job
; Sha na na na na na na na na
; Get a job
; Sha na na na na na na na na
; Wah yip yip yip yip yip yip yip yip yip
; Sha boom

; How many bits are required for the encoding? What is the smallest number of bits that
; would be needed to encode this song if we used a fixed-length code for the eight-symbol
; alphabet?

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))


(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))


(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)   ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (successive-merge (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set)) (cddr leaf-set)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symbol (symbols (left-branch tree))) 
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((element-of-set? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else '())))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))


(define lyrics-tree (generate-huffman-tree (list '(A 2) '(GET 2) '(SHA 3) 
                                                 '(WAH 1) '(BOOM 1) '(JOB 2) 
                                                 '(NA 16) '(YIP 9))))
lyrics-tree

(define lyrics '(GET A JOB 
                     SHA NA NA NA NA NA NA NA NA 
                     GET A JOB 
                     SHA NA NA NA NA NA NA NA NA 
                     WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP 
                     SHA BOOM))
lyrics

(define lyrics-encoded (encode lyrics lyrics-tree))
lyrics-encoded

(decode lyrics-encoded lyrics-tree)

; This method used 84 bits in the encoded message. 
; There are eight symbols in this message so we could use 3 bits to represent all values. There are 36 words in the message so if
; we used a fixed-length encoding we would require 108 bits.

