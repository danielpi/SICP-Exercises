#lang racket

; Exercise 2.42
; The "eight-queens puzzle" asks how to place eight queens on a chessboard so that no queen is in 
; check from any other (i.e., no two queens are in the same row, column, or diagonal). One way to
; solve the puzzle is to work across the board, placing a queen in each column. Once we have placed 
; k - 1 queens, we must place the kth queen in a position where it does not check any of the queens
; already on the board. We can formulate this approach recursively: Assume that we have already
; generated the sequence of all possible ways to place k - 1 queens in the first k - 1 columns of
; the board. For each of these ways, generate an extended set of positions by placing a queen in each
; row of the kth column. Now filter these, keeping only the positions for which the queen in the
; kth column is safe with respect to the other queens. This produces the sequence of all ways to
; place k queens in the first k columns. By continuing this process, we will produce not only one
; solution, but all solutions to the puzzle.

; We implement this solution as a procedure queens, which returns a sequence of all solutions to
; the problem of placing n queens on an n x n chessboard. queens has an internal procedure
; queen-cols that returns the sequence of all ways to place queens in the first k columns of the 
; board.

; In this procedure rest-of-queens is a way to place k - 1 queens in the first k - 1 columns,
; and new-row is a proposed row in which to place the queen for the kth column. Complete the
; program by implementing the representation for sets of board positions, including the procedure
; adjoin-position, which adjoins a new row-column position to a set of positions, and empty-board
; which represents an empty set of positions. You must also write the procedure safe?, which
; determines for a set of positions, whether the queen in the kth column is sage with respect to
; the others. (Note that we need only check whether the new queen is safe - the other queens are
; already guaranteed safe with respect to each other.)

; Generate a sequence of all possible solutions to the puzzle.
; (((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8)) ; one solution
;  ((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8))
;  ((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8))
;  ... )

; The possible postions of placing the first queen in the first column are
; ((1 1) (2 1) (3 1) (4 1) (5 1) (6 1) (7 1) (8 1))

; The possible positions for placing the firt and second queen would be something like
; (((1 1) (3 2)) ((1 1) (4 2)) ... )

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 1 7)

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (flatmap op seq)
  (accumulate append '() (map op seq)))



(define empty-board '() )

(define (place-queen rank file)
  (cons rank file))
(define (queen-rank queen)
  (car queen))
(define (queen-file queen)
  (cdr queen))

(define (adjoin-position rank file board) 
  (cons (place-queen rank file) board))

(define (find-first pred items)
  (cond ((null? items) '())
        ((pred (car items))
         (car items))
        (else (find-first pred (cdr items)))))

(define (safe? file board)
  (define (get-queen-by-file file board)
    (find-first (lambda (queen)
                  (= (queen-file queen) file))
                board))
  (let* ((the-queen (get-queen-by-file file board))
        (other-queens (filter (lambda (q)
                                (not (and (= (queen-rank the-queen)
                                             (queen-rank q))
                                          (= (queen-file the-queen)
                                             (queen-file q)))))
                              board)))
    (and (not (accumulate (lambda (p q)
                            (or q
                                ( = (queen-rank p)
                                    (queen-rank the-queen))))
                          #f
                          other-queens))
         (not (accumulate (lambda (p q)
                            (or q 
                                (= (abs (- (queen-rank the-queen) (queen-rank p)))
                                   (abs (- (queen-file the-queen) (queen-file p))))))
              #f
              other-queens)))))
  

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter 
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)
(accumulate (lambda (sol count) (+ count 1)) 0 (queens 8))

