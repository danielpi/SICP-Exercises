#lang racket

; Exercise 2.71
; Suppose we have a Huffman tree for an alphabet of n symbols, and that the relative
; frequencies of the symbols are 1, 2, 4,..., 2^n-1. Sketch the tree for n = 5; for
; n = 10. In such a tree (for general n) how many bits are required to encode the
; most frequenct symbol? The least frequency symbol?

; For n = 5
; Weights: 1, 2, 4, 8, 16
; Symbols: a, b, c, d,  e

; {a b c d e} 31
;   /       \
; e 16  {a b c d} 15
;        /     \
;      d 8  {a b c} 7
;           /     \
;         c 4   {a b} 3
;                /  \
;              a 1  b 2
 
; Same deal for n = 10?

; Most frequent symbol will require 1 bit.
; leas frequent symbol will require n - 1 bits

; Answer on the web draws the tree in the opposite direction. I don't think there is any
; actual difference here (in my tree e would be represented by 0 as opposed to 1 in the
; solution on the scheme wiki site)

;                    {a b c d e} 31
;                     /           \
;                {a b c d} 15      e 16
;                 /     \
;           {a b c} 7    d 8
;             /    \
;        {a b} 3    c 4
;         /   \
;      a 1    b 2