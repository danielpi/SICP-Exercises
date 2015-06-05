import Foundation

public struct Queen: Printable {
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

public typealias Solution = [Queen]
public typealias Solutions = [Solution]

public func isSafe(a: Queen, b: Queen) -> Bool {
    return (a.row != b.row) &&
        (a.col != b.col) &&
        (abs(a.row - b.row) != abs(a.col - b.col))
}
//isSafe(Queen(2, 1), Queen(8, 7))

public func isSafeFor(queen: Queen) -> (Queen) -> Bool {
    return { q in return isSafe(q, queen) }
}

public func isSafe(queen: Queen, solution: Solution) -> Bool {
    return solution.map(isSafeFor(queen)).reduce(true) { $0 && $1 }
}
//isSafe(Queen(2, 1), [Queen(3, 2), Queen(4, 6)])