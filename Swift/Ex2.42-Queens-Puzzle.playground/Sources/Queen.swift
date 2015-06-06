import Foundation

public struct QueenF: Printable {
    public let row: Int
    public let col: Int
    
    public init(_ row: Int, _ col: Int) {
        self.row = row
        self.col = col
    }
    
    public var description: String {
        return "[\(row),\(col)]"
    }
}

public typealias SolutionF = [QueenF]
public typealias SolutionsF = [SolutionF]

public func isSafeF(a: QueenF, b: QueenF) -> Bool {
    return (a.row != b.row) &&
        (a.col != b.col) &&
        (abs(a.row - b.row) != abs(a.col - b.col))
}
//isSafe(Queen(2, 1), Queen(8, 7))

public func isSafeForF(queen: QueenF) -> (QueenF) -> Bool {
    return { q in return isSafeF(q, queen) }
}

public func isSafeF(queen: QueenF, solution: SolutionF) -> Bool {
    return solution.map(isSafeForF(queen)).reduce(true) { $0 && $1 }
}
//isSafe(Queen(2, 1), [Queen(3, 2), Queen(4, 6)])

public func addColOfQueensF(board: SolutionsF, col: Int) -> SolutionsF {
    let possibleQueens = Array(1...8).map() { row in return QueenF(row, col) }
    
    if board.isEmpty {
        return possibleQueens.map() { [$0] }
    } else {
        let a = possibleQueens.flatMap() { queen in
            board.map() { solution in
                isSafeF(queen, solution) ? solution + [queen] : []
            }
        }
        return a.filter() { !$0.isEmpty }
    }
}
