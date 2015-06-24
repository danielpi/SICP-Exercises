#lang racket


; 2.3 Symbolic Data
; All the compound data objects we have used so far were constructed ultimately from numbers. In
; this section we extend the representational capability of our language by introducing the ability
; to work with arbitrary symbols as data.

; 2.3.1 Quotation
; If we can form compound data using symbols, we can have lists such as 

; (a b c d)
; (23 45 17)
; ((Norah 12) (Molly 9) (Anna 7) (Lauren 6) (Charlotte 4))

; Lists containing symbols can look just like the expressions of our language:

; (* (+ 23 45)
;    (+ x 9))

(display "(fact n)")(newline)
(define (fact n)
  (if (= n 1) 1 (* n (fact (- n 1)))))

(fact 1)
(fact 2)
(fact 3)
(fact 4)

; In order to manipulate symbols we need a new element in our language: the ability to quote a data 
; object. Suppose we want to construct the list (a, b). We can't accomplish this with (list a b), 
; because this expression constructs a list of the values of a and n rather than the symbols 
; themselves. This issue is well known in the context of natural languages, where words and sentences
; may be regarded either as semantic entities or as character strings (syntactic entities). The 
; common practice in natural languages is to use quotation marks to indicate that a work or a 
; sentence is to be treated literally as a string of characters. For instance, the first letter of
; "John" is clearly "J". If we tell somebody "say your name aloud," we expect to hear that person's
; name. However if we tell somebody "say 'your name' aloud," we expect to hear the words "your name."
; Note that we are forced to nest quotation marks to describe what somebody else might say.

; We can follow this same practice to identify lists and symbols that are to be treated as data
; objects rather than as expressions to be evaluated. However, our format for quoting differs from 
; that of natural languages in that we place a quotation mark (traditionally, the single quote
; symbol ') only at the begining of the object to be quoted. We can get away with this in Scheme
; syntax because we rely on blanks and parentheses to delimit objects. Thus, the meaning of the 
; single quote character is to quote the next object.

; Now we can distinguish between symbols and their values

(display "Now we can distinguish between symbols and their values")(newline)
(define a 1)
(define b 2)
(list a b)
(list 'a 'b)

; Quotation also allows us to type in compound objects, using the conventional printed representation
; for lists:

(display "compound objects")(newline)
(car '(a b c))
(cdr '(a b c))

; In keeping with this, we can obtain the empty list by evaluating '() and thus dispense with the
; variable nil.

; One additionaly primitive used in manipulating symbols is eq?, which takes two symbols as
; arguments and tests whether they are the same. Using eq?, we can implement a useful procedure
; called memq. This takes two arguments, a symbol and a list. If the symbol is not contained in the
; list (i.e., is not eq? to an item in the list), then memq returns false. Otherwise, it returns the 
; sublist of the list beginning with the first occurance of the symbol:

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; For example, the value of

(display "(memq 'apple '(pear banana prune))") (newline)
(memq 'apple '(pear banana prune))

; is false, whereas the value of

(display "(memq 'apple '(x (apple sauce) y apple pear))") (newline)
(memq 'apple '(x (apple sauce) y apple pear))

; is (apple pear)

(memq 'apple '(x y (apple blah) blue cheese apple))







