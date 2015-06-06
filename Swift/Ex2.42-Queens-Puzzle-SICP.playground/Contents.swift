import Cocoa

// Exercise 2.42
// The "eight-queens puzzle" asks how to place eight wueens on a chessboard so that no queen is in check from any other (i.e., no two queens are in the same row, column, or diagonal). One way to solve the puzzle is to work across the board, placing a queen in each column. Once we have placed k - 1 queens, we must place the kth queen in a position where it does not check any of the queens already on the board.

// We can formulate this approach recursively:
// - Assume that we have already generated the sequence of all possible ways to place k - 1 queens in the first k - 1 columns of the board.
// - For each of these ways, generate an extended set of positions by placing a queen in each row of the kth column.
// - Now filter these, keeping only the positions for which the queen in the kth column is safe with respect to the other queens.
// This produces the sequence of all ways to place k queens in the first k columns. By continuing this process, we will produce not only one solution, but all solutions to the puzzle.

// We implement this solution as a procedure queens, which returns a sequence of all solutions to the problem of placing n queens on an n x n chessboard. queens has an internal procedure queen-cols that returns the sequence of all ways to place queens in the first k columns of the board.

// In this procedure rest-of-queens is a way to place k - 1 queens in the first k - 1 columns, and new-row is a proposed row in which to place the queen for the kth column. Complete the program by implementing the representation for sets of board positions, including the procedure adjoin-position, which adjoins a new row-column position to a set of positions, and empty-board, which represents an empty set of positions. You must also write the procedure safe?, which determines for a set of positions, whether the queen in the kth column is safe with respect to the others. (Note that we need only check whether the new queen is safe - the other queens are already guaranteed safe with respect to each other).

struct Pair<A,B> {
    let left: A
    let right: B
}

func cons<A,B>(left: A, right: B) -> Pair<A,B> {
    return Pair(left: left, right: right)
}
func car<A,B>(pair: Pair<A,B>) -> A {
    return pair.left
}
func cdr<A,B>(pair: Pair<A,B>) -> B {
    return pair.right
}

func cons<A>(value: A, list: [A]) -> [A] {
    var newList = list
    newList.insert(value, atIndex: 0)
    return newList
}
func car<A>(list:[A]) -> A {
    return list[0]
}
func cdr<A>(list:[A]) -> [A] {
    return Array(list[1..<list.count])
}

func enumerateInterval(low: Int, high: Int) -> [Int] {
    return Array(low...high)
}

typealias Queen = Pair<Int,Int>

let emptyBoard = []

func placeQueen(rank: Int, file: Int) -> Queen {
    return cons(rank, file)
}
func queenRank(queen: Queen) -> Int {
    return car(queen)
}
func queenFile(queen: Queen) -> Int {
    return cdr(queen)
}

func adjoinPosition(rank: Int, file: Int, board: [Queen]) -> [Queen] {
    return cons(placeQueen(rank, file), board)
}

func findFirst(pred: (Queen) -> Bool, items: [Queen]) -> Queen {
    switch true {
    case items.isEmpty:
        return Queen(left: 0, right: 0)
    case pred(car(items)):
        return car(items)
    default:
        return findFirst(pred, cdr(items))
    }
}

func isSafe(file: Int, board: [Queen]) -> Bool {
    func getQueenByFile(file: Int, board: [Queen]) -> Queen {
        return findFirst({ queen in
            queenFile(queen) == file
            }, board)
    }
    
    let theQueen = getQueenByFile(file, board)
    let otherQueens = filter(board) { q in
           !(queenRank(theQueen) == queenRank(q)) &&
            (queenFile(theQueen) == queenFile(q))
    }
    
    let a = otherQueens.reduce(false) { (q,p) in q || (queenRank(p) == queenRank(theQueen)) }
    let b = otherQueens.reduce(false) { (q,p) in q || (abs(queenRank(theQueen) - queenRank(p)) == (abs(queenFile(theQueen) - queenFile(p)))) }
    
    return !a && !b
}

func queens(boardSize: Int) -> [[Queen]] {
    var queenCols: (Int) -> [[Queen]] = { _ in [[Queen]]() }
    queenCols = { k in
        if k == 0 {
            return [[Queen]]()
        } else {
            let a = flatMap(queenCols(k - 1), { restOfQueens in map(enumerateInterval(1, boardSize), { newRow in adjoinPosition(newRow, k, restOfQueens) }) })
            
            return a.filter() { positions in isSafe(k, positions) }
        }
    }
    return queenCols(boardSize)
}

queens(8)













