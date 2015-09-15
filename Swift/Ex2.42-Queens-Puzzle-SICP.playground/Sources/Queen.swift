import Foundation

public struct Pair<A,B> {
    let left: A
    let right: B
    
    public init(left: A, right: B) {
        self.left = left
        self.right = right
    }
}

public func cons<A,B>(left: A, right: B) -> Pair<A,B> {
    return Pair(left: left, right: right)
}
public func car<A,B>(pair: Pair<A,B>) -> A {
    return pair.left
}
public func cdr<A,B>(pair: Pair<A,B>) -> B {
    return pair.right
}

public func cons<A>(value: A, list: [A]) -> [A] {
    var newList = list
    newList.insert(value, atIndex: 0)
    return newList
}
public func car<A>(list:[A]) -> A {
    return list[0]
}
public func cdr<A>(list:[A]) -> [A] {
    return Array(list[1..<list.count])
}


public typealias Queen = Pair<Int,Int>


public func enumerateInterval(low: Int, high: Int) -> [Int] {
    return Array(low...high)
}


public let emptyBoard: [Queen] = []

func placeQueen(rank: Int, file: Int) -> Queen {
    return cons(rank, right: file)
}
func queenRank(queen: Queen) -> Int {
    return car(queen)
}
func queenFile(queen: Queen) -> Int {
    return cdr(queen)
}

public func adjoinPosition(rank: Int, file: Int, board: [Queen]) -> [Queen] {
    return cons(placeQueen(rank, file: file), list: board)
}

func findFirst(pred: (Queen) -> Bool, items: [Queen]) -> Queen {
    switch true {
    case items.isEmpty:
        return Queen(left: 0, right: 0)
    case pred(car(items)):
        return car(items)
    default:
        return findFirst(pred, items: cdr(items))
    }
}

public func isSafe(file: Int, board: [Queen]) -> Bool {
    func getQueenByFile(file: Int, board: [Queen]) -> Queen {
        return findFirst({ queen in
            queenFile(queen) == file
            }, items: board)
    }
    
    let theQueen = getQueenByFile(file, board: board)
    let otherQueens = board.filter { q in
        !((queenRank(theQueen) == queenRank(q)) &&
            (queenFile(theQueen) == queenFile(q)))
    }
    
    let a = otherQueens.reduce(false) { (q,p) in q || (queenRank(p) == queenRank(theQueen)) }
    let b = otherQueens.reduce(false) { (q,p) in q || (abs(queenRank(theQueen) - queenRank(p)) == (abs(queenFile(theQueen) - queenFile(p)))) }
    
    return !a && !b
}


public func queensF(boardSize: Int) -> [[Queen]] {
    var queenCols: (Int) -> [[Queen]] = { _ in [[Queen]]() }
    queenCols = { k in
        if k == 0 {
            
            return [emptyBoard]
        } else {
            let a = queenCols(k - 1).flatMap({ restOfQueens in
                enumerateInterval(1, high: boardSize).map({ newRow in
                    adjoinPosition(newRow, file: k, board: restOfQueens)
                })
            })
            
            return a.filter() { positions in isSafe(k, positions) }
            //return a
        }
    }
    return queenCols(boardSize)
}
