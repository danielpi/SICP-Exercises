import Cocoa

// Exercise 2.43
// Louis Reasoner is having a terrible time doing Exercise 2.42. His queens procedure seems to work but it runs extremely slowly. (Louis never does manage to wait long enough for it to solve even the 6x6 case.) When Louis asks Eva Lu Ator for help, she points out that he has interchanged the order of the nested mappings in the flatmap, writing it as

public func queens42(boardSize: Int) -> [[Queen]] {
    var queenCols: (Int) -> [[Queen]] = { _ in [[Queen]]() }
    queenCols = { k in
        if k == 0 {
            return [emptyBoard]
        } else {
            let a = flatMap(queenCols(k - 1), { restOfQueens in     // 8
                map(enumerateInterval(1, boardSize), { newRow in    // 1965
                    adjoinPosition(newRow, k, restOfQueens)         // 15720
                })
            })
            
            return a.filter() { positions in isSafe(k, positions) } // 15728
            //return a
        }
    }
    return queenCols(boardSize)
}

let boardsize = 4
let solutions42 = queens42(boardsize)
solutions42.count

let a = pow(5.0, 8.0)
a

public func queens43(boardSize: Int) -> [[Queen]] {
    var queenCols: (Int) -> [[Queen]] = { _ in [[Queen]]() }
    queenCols = { k in
        if k == 0 {
            return [emptyBoard]
        } else {
            let a = flatMap(enumerateInterval(1, boardSize), { newRow in  //
                map(queenCols(k - 1), { restOfQueens in                   //
                    adjoinPosition(newRow, k, restOfQueens)               //
                })
            })
            
            return a.filter() { positions in isSafe(k, positions) }       //
            //return a
        }
    }
    return queenCols(boardSize)
}

let solutions43 = queens43(boardsize)
solutions43.count

// With Louis's solution the recursion occurs in the inner loop.
// enumerateInterval(1, boardSize) = 8, queenCols(k - 1)

// Exchanging the order of the mapping in the flatmap results in queen-cols being re-evaluated for every item in enumerateInterval(1, boardSize). Therefore the whole work has to be duplicated board-size times at every recursion level. Since there are always board-size recursions this means that the whole work will be duplicated board-size^board-size times.

// For the original ordering with a board size of 3, we recurse three times and perform ~ 6 (would be 9 but we exclude illegal queen positions) steps per recursion -> 21 steps
// For a board size of 4 we recurse 4 times and perform ~15 (4^4 = 256 less illegal steps) steps per recursion -> 64 steps

// boardsize^boardsize * T which for 8 works out to 
pow(8.0, 8.0)



