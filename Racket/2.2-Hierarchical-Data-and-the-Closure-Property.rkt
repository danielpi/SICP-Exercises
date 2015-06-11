#lang racket

; 2.2 Hierarchical Data and the Closure Property

; As we have seen , pairs provide a primitive "glue" that we can use to construct compound data 
; objects. Cons can be used to combine not only numbers but pairs as well. As a consequence, pairs
; provide a universal building block from which we can construct all sorts of data structures.

; The ability to create pairs whose elements are pairs is the essence of list structure's importance
; as a representational tool. We refer to this ability as the closure property of cons. In general,
; an operation for combining data objects satisfies the closure property if the results of combining 
; things with that operation can themselves be combined using the same operation. Closure is the key
; to power in any means of combination because it permits us to create hierarchical structures --
; structures made up of parts, which themselves are made up of parts, and so on.


; 2.2.1 Representing Sequences
; One of the useful structures we can build with pairs is a sequence -- an ordered collection of
; data objects. There are, of course, many ways to represent sequences in terms of pairs. One
; particularly striaghtforward representation is shown below for the sequence 1, 2, 3, 4.

(cons 1
      (cons 2
            (cons 3
                  (cons 4 '()))))

; The car of each pair is the corresponding item in the chain, and the cdr of the pair is the next
; pair in the chain. The cdr of the final pair signals the end of the sequence by pointing to a
; distinguished value that is not a pair (nil in scheme or empty list in Racket '())

; Such a sequence of pairs, formed by nested conses is called a list. There is a primitive for such
; a construct called list

(list 1 2 3 4)

(define one-through-four (list 1 2 3 4))
one-through-four

; We can think of car as selecting the first item in the list, and of cdr as selecting the sublist 
; consisting of all but the first item. Nested applications of car and cdr can be used to extract
; the second, third, and subsequent items in the list. The constructor cons makes a list like the 
; original one but with an additional item at the beginning.

(car one-through-four)

(cdr one-through-four)
(car (cdr one-through-four))

(cons 10 one-through-four)
(cons 5 one-through-four)


; List operations
; The use of pairs to represent sequences of elements as lists is accompanied by conventional
; programming techniques for manipulating lists by successively "cdring down" the lists. For example
; the procedure list-ref takes as arguments a list and a number n and returns the nth item of the list.
; It is customary to number the elements of the list beginning with 0. The method for computing
; list-ref is the following:
; - For n = o, list-ref should return the car of the list
; - Otherwise, list-ref shold return the (n-1)st item of the cdr of the list

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
(list-ref squares 3)

; Often we cdr down the whole list. To aid in this, Racket includes a primitive predicate null?,
; which tests whether its argument is the empty list. The procedure length, which returns the number 
; of items in a list, illustrates this typical pattern of use:

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

(length odds)

; The length procedure implements a simple recursive plan. The reduction step is:
; - The length of any list is 1 plus the length of the cdr of the list

; This is applied successively until we reach the base case:
; - The length of the empty list is 0.

; We could also compute length in an iterative style:

(define (length2 items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))
(length2 odds)

; Another conventional programming technique is to "cons up" an answer list while cdring down a
; list, as in the procedure append, which takes two lists as arguments and combines their elements
; to make a new list:

(append squares odds)
(append odds squares)

; Append is also implmented using a recursive plan. To append lists list1 and list2, do the following
; - If list1 is the empty list, then the result is just list2
; - Otherwise, append the cdr of list1 and list 2, and cons the car of list1 onto the result

(define (append2 list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
(append2 squares odds)
(append2 odds squares)


; Mapping over lists
; One extremely useful operation is to apply some transformation to each element in a list
; and generate the list of results. For instance, the following procedure scales each number
; in a list by a given factor

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))
(scale-list (list 1 2 3 4 5) 10)
; (10 20 30 40 50)

; We can abstract this general idea and capture it as a common pattern expressed as a higher-order
; procedure, just as in section 1.3. The higher-order procedure here is called map. Map takes as
; arguments a procedure of one argument and a list, and returns a list of the results produced
; by applying the procedure to each element in the list

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))
(map abs (list -10 2.5 -11.6 17))
; (10 2.5 11.6 17)

(map (lambda (x) (* x x))
     (list 1 2 3 4))
; (1 4 9 16)

; Now we can give a new definition of scale-list in terms of map

(define (scale-list2 items factor)
  (map (lambda (x) (* x factor))
       items))
(scale-list2 (list 1 2 3 4 5) 10)
; (10 20 30 40 50)

; Map is an important construct, not only because it captures a common pattern, but because it
; establishes a higherlevel of abstraction in dealing with lists. In the original definition of 
; scale-list, the recursive structure of the program draws attention to the element-by-element
; processing of the list. Defining scale-list2 in terms of map suppresses that level of detail
; and emphasizes that scaling transforms a list of elements to a list of results. The difference
; between the two definitions is not that the computer is performing a different process (it isn't)
; but that we think about the process differently. In effect, map helps establish an abstraction
; barrier that isolates the implementation of procedures that transform lists from the details
; of how the elements of the list are extracted and combined. Like the barriers shown in figure
; 2.1, this abstraction gives us the flexibility to change the los-level details of how sequences
; are implemented, while preserving the conceptual framework of operations that transform
; sequences to sequences. Section 2.2.3 expands on this use of sequences as a framework for
; organizing programs.


; 2.2.2 Hierachical Structures
; The representation of sequences in terms of lists generalizes naturally to represent sequences
; whose elements may themselves be sequences. For example, we can regard the object (1 2) 3 4)
; constructed by

(cons (list 1 2) (list 3 4))

; as a list of three items, the first of which is itself a list, (1 2). Indeed, this is suggested 
; by the form in which the result is printed by the interpreter. 

; Another way to think of sequences whose elements are sequences is as trees. The elements of
; the sequence are the branches of the tree, and elements that are themselves sequences are 
; subtrees. 

; Recursion is a natural tool for dealing with tree structures, since we can often reduce
; operations on trees to operations on their branches, which reduce to operations on the branches
; of the branches, and so on, until we reach the leaves of the tree. As an example, compare the
; length procedure of section 2.2.1 with the count-leaves procedure, which returns the total
; number of leaves of a tree

; To implement count-leaves, recall the recursive plan for computing length:
; - Length of a list x is 1 plus length of the cdr of x
; - Length of the empty list is 0

; Count-leaves is similar. The value for the empty list is the same:
; - Count-leaves of the empty list is 0

; But in the reduction step, where we strip off the car of the list, we must take into account
; that the car may itself be a tree whose leaves we need to count. Thus, the appropriate 
; reduction step is
; - Count-leaves of a tree x is count-leaves of the car of c plus count-leaves of the cdr of x

; Finally by taking cars we reach actual leaves, so we need another base case:
; - Count-leaves of a leaf is 1

; To aid in writing recursive procedures on trees, Scheme provides the primitive predicate
; pair?, which tests whether its argument is a pair. Here is the complete procedure

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))
(length x) ; 3
(count-leaves x) ; 4

(list x x) ; (((1 2) 3 4) ((1 2) 3 4))
(length (list x x)) ; 2

(count-leaves (list x x)) ; 8


; Mapping over trees
; Just as map is a powerful abstraction for dealing with sequences, map together with recursion 
; is a powerful abstraction for dealing with trees. For instance, the scale-tree procedure, analogous
; to scale-list of section 2.2.1, takes as arguments a numeric factor and a tree whose leaves
; are numbers. It returns a tree of the same shape, where each number is multiplied by the factor.
; The recursive plan for scale-tree is similar to the one for count-leaves:

(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

; Another way to implement scale-tree is to regard the tree as a sequence of sub-trees and use
; map. We map over the sequence, scaling each sub-tree in turn, and return the list of results. In
; the base case, where the tree is a leaf, we simply multiply by the factor:

(define (scale-tree2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree2 sub-tree factor)
             (* sub-tree factor)))
       tree))
(scale-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

; Many tree operations can be implemented by similar combinations of sequence operations and
; recursion.


; Sequences as Conventional Interfaces
; In working with compound data, we've stressed how data abstraction permits us to design programs
; without becoming enmeshed in the details of data representations, and how abstraction preserves
; for us the flexibility to experiment with alternative representations. In this section, we
; introduce another powerful design principle for working with data structures - the use of 
; conventional interfaces.

; In section 1.3 we saw how program abstractions, implemented as higher-order procedures, can 
; capture common patterns in programs that deal with numerical data. Our ability to formulate 
; analogous operations for working with compound data depends crucially on the style in which
; we manipulate our data structures. Consider, for example, the following procedure, analogous
; to the count-leaves procedure of sections 2.2.2, which takes a tree as argument and computes 
; the sum of the squares of the leaves that are odd:

(define (square x) (* x x))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(sum-odd-squares (list 1 2 3 (list 4 5) (list 6 7 8 9)))

; On the surface, this procedure is very different from the following one, which constructs a 
; list of all the even Fibonacci numbers Fib(k), where k is less than or equal to a given 
; integer n:

(define (fib n)
  (cond ((= n 0) 0)
         ((= n 1) 1)
         (else (+ (fib (- n 1))
                  (fib (- n 2))))))

(define (even-fibs n)
   (define (next k)
     (if (> k n)
         '()
         (let ((f (fib k)))
           (if (even? f)
               (cons f (next (+ k 1)))
               (next (+ k 1))))))
   (next 0))

(display "even-fibs")
(even-fibs 10)
; Despite the fact that these two procedures are structurally very different, a more abstract 
; description of the two computations reveals a great deal of similarity. The first program
;  - enumerates the leaves of a tree
;  - filters them, selecting the odd ones
;  - squares each of the selected ones and
;  - accumulates the results using +, starting with 0

; The second program
;  - enumerates the integers from 0 to n
;  - computes the Fibonacci number for each integer
;  - filters them, selecting the even ones and
;  - accumulates the results using cons, starting with the empty list

; A signal processing engineer would find it natural to conceptualize these processes in terms
; of signals flowing through a cascade of stages, each of which implements part of the program 
; plan, as shown in Figure 2.7. In sum-odd-squares, we begin with an enumerator, which generates
; a "signal" consisting of the leaves of a given tree. This signal is passed through a filter, 
; which eliminates all but the odd elements. The resulting signal is in turn passed through a 
; map, which is a "transducer" that applies the square procedure to each element. The output of
; the map is then fed to an accumulator, which combines the elements using +, starting from an 
; initial 0. The plan for even-fibs is analogous.

; Unfortunately, the two procedure definitions above fail to exhibit this signal-flow structure. 
; For instance, if we examine the sum-odd-squares procedure, we find that the enumeration is
; implemented partly by the null? and pair? tests and partly by the tree-recursive structure of
; the procedure. Similarly, the accumulation is found partly in the tests and partly in the
; addition used in the recursion. In general, there are no distinct parts of either procedure
; that correspond to the elements in the signal-flow description. Our two procedures decompose
; the computations in a different way, spreading the enumeration over the program and mingling it 
; with the map, the filter and the accumulation. If we could organize our programs to make
; the signal-flow structure manifest in the rocedures we write, this would increase the conceptual
; clarity of the resulting code.


; Sequence Operations
; The key to organising programs so as to more clearly reflect the signal-flow structure is to 
; concentrate on the "signals" that flow from one stage in the process to the next. If we represent 
; these signals as lists, then we can use list operations to implement the processing at each of 
; the stages. For instance, we can implement the mpping stages of the signal-flow diagrams using 
; the map procedure from section 2.2.1:

(map square (list 1 2 3 4 5))

; Filtering a sequence to select only those elements that satisfy a given predicate is accomplished
; by

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))

; Accumulations can be implemented by

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons '() (list 1 2 3 4 5))
  
; All that remains to implement signal-flow diagrams is to enumerate the sequence of elements to
; be processed. For even-fips, we need to generate the sequence of integers in a given range,
; which we can do as follows:

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)

; To enumerate the leaves of a tree, we can use

(define (enumerate-tree tree)
  (cond ((null? tree) '())
       ((not (pair? tree)) (list tree))
       (else (append (enumerate-tree (car tree))
                     (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

; Now we can reformulate sum-odd-squares and even-fibs as in the signal-flow diagrams. For
; sum-odd-squares, we enumerate the sequence of leaves of the tree, filter this to keep only
; odd numbers in the sequence, square each element, and sum the results:

(define (sum-odd-squares2 tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(sum-odd-squares2 (list 1 2 3 (list 4 5) (list 6 7 8 9)))

; For even-fibs, we enumerate the integers from 0 to n, generate the fibonacci number for each
; of these integers, filter the resulting sequence to keep only the even elements, and accumulate
; the results into a list:

(define (even-fibs2 n)
  (accumulate cons '() (filter even? (map fib (enumerate-interval 0 n)))))

(even-fibs2 15)

; The value of expressing programs as sequence operations is that this helps us make program 
; designs that are modular, that is, designs that are constructed by combining relatively 
; independent pieces. We can encourage modular design by providing a library of standard components
; together with a conventional interface for connecting the components in flexible ways.

; Modular construction is a powerful strategy for controlling complexity in engineering design.
; In real signal-processing applications, for example, designers regularly build systems by cascading
; elements selected from standardized families of filters and transducers. Similarly, sequence
; operations provide a library of standard program elements that we can mix and match. For 
; instance, we can reuse pieces from the sum-odd-squares and even-fibs procedures in a program
; that constructs a list of the squares of the first n + 1 Fibonacci numbers:

(define (list-fib-squares n)
  (accumulate cons '() (map square (map fib (enumerate-interval 0 n)))))
(list-fib-squares 10)

; We can rearrange the pieces and use them in computing the product of the squares of the odd integers
; in a sequence

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))
(product-of-squares-of-odd-elements (list 1 2 3 4 5))

; We can also formulate conventional data-processing applications in terms of sequence operations. Suppose
; we have a sequence of personnel records and we want to find the salary of the highest-paid programmer.
; Assume that we have a selector salary that returns the salary of a record, and a predicate programmer? that 
; tests if a record is for a programmer. Then we can write

; (define (salary-of-highest-paid-programmer records)
;   (accumulate max 0 (map salary (filter programmer? records))))

; These examples give just a hint of the vast range of operations that can be expressed as sequence operations.

; Sequences, implemented here as lists, serve as a conventional interface that permits us to combine processing
; modules. Additionally, when we uniformly represent structures as sequences, we have localized the data-structure 
; dependencies in our programs to a small number of sequence operations. By changing these, we can experiment with
; alternative representations of sequences, while leaving the overall design of our programs intact. We will 
; exploit this capability in section 3.5, when we generalise the sequence-processing paradigm to admit infinite
; sequences.


; Nested Mappings
; We can extend the sequence paradigm to include many computations that are commonly expressed
; using nested loops. Consider this problem: Given a positive integer n, find all ordered pairs
; of distinct positive integers i and j, where 1 <= j <= i <= n, such that i + j is prime. For
; example if n = 6, then the pairs are the following

;   i   | 2 3 4 4 5 6 6
;   j   | 1 2 1 3 2 1 5
; -------------------------
; i + j | 3 5 5 7 7 7 11

; A natural way to organise this computation is to generate the sequence of all ordered pairs of
; positive intergers less than or equal to n, filter to select those whose sum is prime, and then,
; for each pair (i, j) that passes through the filter, produce the triple (i, j, i + j)

; Here is a way to generate the sequence of pairs:
; - For each integer i <= n, 
;  - enumerate the integers j < i, and
;  - for each such i and j generate the pair (i, j).

; In terms of sequence operations,
; - map along the sequence (enumerate-interval 1 n). 
; - For each i in this sequence we map along the sequence (enumerate-interval 1 (- i 1)).
; - For each j in this latter sequence, we generate the pair (list i j). 
; This gives us a sequence of pairs for each i. Combining all the sequences for all the i 
; (by accumulating with append) produces the required sequence of pairs

(define (ordered-pairs-of-integers n)
  (accumulate 
   append '() (map (lambda (i) 
                     (map (lambda (j) (list i j)) 
                          (enumerate-interval 1 (- i 1)))) 
                   (enumerate-interval 1 n))))

(ordered-pairs-of-integers 6)

; The combination of mapping and accumulating with append is so common in this sort of program
; that we will isolate it as a separate procedure

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

; Now filter this sequence of pairs to find those whose sum is prime. The filter predicate
; is called for each element of the sequence; its argument is a pair and it must extract the
; integers from the pair. Thus, the predicate to apply to each element in the sequence is

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

; We can test if a number is prime if it's smallest divisor is itself

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(prime-sum? (list 3 4))

; Finally, generate the sequence of results by mapping over the filtered pairs using the following 
; procedure, which constructs a tripple consisting of the two elements of the pair along with
; their sum:

(define (make-pair-sum pair) 
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

; Combining all these steps yeilds the complete procedure

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j) (list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))
(prime-sum-pairs 6)
  
; Nested mappings are also useful for sequences other than those that enumerate intervals. Suppose we
; wish to generate all the permutations of a set S; that is, all the ways of ordering the items in the
; set. For instance, the permutations of {1,2,3} are {1,2,3},{1,3,2},{2,1,3},{2,3,1},{3,1,2} and {3,2,1}.
; Here is a plan for generating the permutations of S: For each item x in S, recursively generate the sequence
; of permuttions of S - x, and adjoin x to the front of each one. This yeilds, for each x in S, the sequence of 
; permutations of S that begin with x. Combining these sequences for all x gives all the permutations of S:

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))
(permutations (list 1 2 3))
  

; Notice how this strategy reduces the problem of generating permutations of S to the problem of 
; generating the the permutations of sets with fewer elements than S. In the terminal case, we
; work our way down to the empty list, which represents a set of no elements. For this, we generate
; (list '()), which is a sequence with one item, namely the set with no elements. The remove
; procedure used in permutations returns all the items in a given sequence except for a given item.
; This can be expressed as a simple filter

(define (remove2 item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))
(remove2 3 (list 1 2 3 4 5))



; 2.2.4 Example: A Picture Language
(display "2.2.4 Example: A Picture Language") (newline)

; This section presents a simple language for drawing pictures that illustrates the power of data
; abstractionand closure, and also exploits higher-order procedures in an essential way. The language
; is designed to make it easy to experiment with patterns, which are composed of repeated elements
; that are shifted and scaled. In this language, the data objects being combined are represented as
; procedures rather than as list structure. Just as cons , which satisfies the closure property, 
; allowed us to easily build arbitrarily complicated list structure, the operations in this languag
; whic also satisfy the closure property, allow us to build arbitrarily complicated patterns.

; The picture language
; When we began our study of programming in Section 1.1, we emphasized the importance of describing
; a language by focusing on the language's primitives, its means of combination, and its means of
; abstraction. We'll follow that framework here.

; Part of the elegance of this picture language is that there is only one kind of element, called
; a painter. A painter draws an image that is shifted and scaled to fit within a designated 
; parallelogram-shaped frame. For example, there's a primitive painter we'll call wave that makes
; a crude line drawing. The actual shape of the drawing depends on the frame - all four images in
; figure 2.10 are produced by the same wave painter, but with respect to four different frames.
; Painters can be more elaborate than this: The primitive painter called rogers paints a picture of 
; MIT's founder, William Barton Rogers, as shown in Figure 2.11. The four images in figure 2.11 are 
; drawn with respect to the same four frames as the wave images in figure 2.10.

; To combine images, we use various operations that construct new painters from given painters. For
; example, the beside operation takes two painters and produces a new, compound painter that draws
; the first painter's image in the left half of the frame and the second painter's image in the right
; half of the frame. Similary, below takes two painters and produces a compound painter that draws
; the first painter's image below the second painter's image. Some operations transform a single
; painter to produce a new painter. For example, flip-vert takes a painter and produces a painter
; that draws its image upside down, and flip-horiz produces a painter that draws the original painter's
; image left-to-right reversed.

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

; In building up a complex image in this manner we are exploiting the fact that painters are closed
; under the language's means of combination. The beside or below of two painters is itself a painter; 
; therefore, we can use it as an element in making more complex painters. As with building up list 
; structure using cons, the closure of our data under the means of combination is crucial to the 
; ability to create complex structures while using only a few operations.

; Once we can combine painters, we would like to be able to abstract typical patterns of combining
; painters. We will implement the painter operations as Scheme procedures. This means that we don't 
; need a special abstraction mechanism in the picture language: Since the means of combination are 
; ordinary Scheme procedures, we automatically have the capability to do anything with painter
; operations that we can do with procedures. For example, we can abstract the pattern in wave4 as

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

; and define wave4 as an instance of this pattern

; (define wave4 (flipped-pairs wave))

; We can also define recursive operations. Here's one that makes painters split and branch towards 
; the right.

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

; We can produce balanced patterns by branching upwards as well as towards the right.

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

; By placing four copies of a corner-split appropriately, we obtain a pattern called square-limit,
; whose application to wave and rogers is shown in Figure 2.9:

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
        (let ((half (beside (flip-horiz quarter) quarter)))
          (below (flip-vert half) half))))


; Higher-order operations
; In addition to abstracting patterns of combining painters, we can work at a higher level, abstracting
; patterns of combining painter operations. That is, we can view the painter operations as elements to
; manipulate and can write means of combination for these elements-procedures that take painter operations 
; as arguments and create new painter operations.

; For example, flipped-pairs and square-limit each arrange four copies of a painter's image in a square
; patter; they differ only in how they orient the copies. One way to abstract this pattern of painter 
; combination is with the following procedure, which takes four one-argument painter operations and 
; produces a painter operation that transforms a given painter with those four operations and arranges
; the results in a square. tl,tr,bl and br are the transformations to apply to the top left copy, the 
; top right copy, the bottom left copy, and the bottom right copy respectively.

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

; Then flipped-pairs can be defined in terms of square-of-four as follows:

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

; and square-limit can be expressed as

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


; Frames
; Before we can show how to implement painters and their means of combination, we must first 
; consider frames. A frame can be described by three vectors - an origin vector and twu edge
; vectors. The origin vector specifies the offset of the frame's origin from some absolute
; origin in the plane, and the edge vectors specify the offsets of the frame's corners from 
; its origin. If the edges are perpendicular, the frame will be rectangular. Otherwise the frame
; will be a more general parallelogram.

; In accordance with data abstraction we need not be specific yet about how frames are represented,
; other than to say that there is a constructor make-frame, which takes three vectors and produces
; a frame, and three corresponding selectors origin-frame, edge1-frame, and edge2-frame.

; We will use coordinates in the unit square (0 <= x, y <= 1) to specify images. With each frame,
; we associate a frame coordinate map, which will be used to shift and scale images to fit the 
; frame. The map transforms the unit square into the frame by mapping the vector v = (x,y) to the
; vector sum

;     Origin(Frame) + (x * Edge1(Frame)) + (y * Edge2(Frame))

; For example, (0,0) is mapped to the origin of the frame, (1,1) to the vertex diagonally opposite
; the origin, and (0.5,0.5) to the center of the frame. We can create a frame's coordinate map
; with the following procedure.

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vert (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

; Observe that applying frame-coord-map to a frame returns a procedure that, given a vector, 
; returns a vector. If the argument vector is in the unit square, the result vector will be in 
; the frame. For example,

((frame-coord-map a-frame) (make-vect 0 0))

; returns the same vector as 

(origin-frame a-frame)














