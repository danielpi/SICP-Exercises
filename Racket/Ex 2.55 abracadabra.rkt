#lang racket

; Eercise 2.55
; Eva Lu Ator types to the interpreter the expression

(car ''abracadabra)

; To her surprise, the interpreter prints back quote. Explain

''abracadabra ; is really 
(quote (quote abracadabra))

; which can also be written as

(list 'quote (quote abracadabra))

; So car will take the first item from a list which in this case is the symbol 'quote

