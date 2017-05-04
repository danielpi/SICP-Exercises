import Cocoa

//: ## Exercise 2.66
//: Implement the lookup procedure for the case where the set of records is structured as a binary tree, ordered by the numerical values of the keys.

extension Array {
    var match: (head: Element, tail: [Element])? {
        return (count > 0) ? (self[0], Array(self[1..<count])) : nil
    }
}

indirect enum TreeSet<T>: CustomStringConvertible {
    case Empty
    case Tree(entry:T, left:TreeSet<T>, right: TreeSet<T>)
    
    var description : String {
        switch self {
        case .Empty:
            return "()"
        case let .Tree(entry, left, right):
            return "(\(entry) \(left) \(right))"
        }
    }
}

func entry<T>(_ tree: TreeSet<T>) -> T {
    switch tree {
    case let .Tree(entry, _, _):
        return entry
    default:
        fatalError("Tried to read an entry from an empty tree")
    }
}

func leftBranch<T>(_ tree: TreeSet<T>) -> TreeSet<T> {
    switch tree {
    case let .Tree(_, left, _):
        return left
    default:
        fatalError("Tried to read the left branch from an empty tree")
    }
}

func rightBranch<T>(_ tree: TreeSet<T>) -> TreeSet<T> {
    switch tree {
    case let .Tree(_, _, right):
        return right
    default:
        fatalError("Tried to read the right branch from an empty tree")
    }
}

func makeTree<T>(_ entry: T, _ left:TreeSet<T>, _ right:TreeSet<T>) -> TreeSet<T> {
    return TreeSet.Tree(entry: entry, left: left, right: right)
}

struct Record: CustomStringConvertible {
    var key: Int
    var value: String
    
    init(_ key: Int, _ value: String) {
        self.key = key
        self.value = value
    }
    
    var description: String {
        return "\(key):\(value)"
    }
}

typealias Records = TreeSet<Record>

func lookup(_ key: Int, _ records: Records) -> Record? {
    switch records {
    case .Empty:
        return nil
    case let .Tree(entry, _, _) where entry.key == key:
        return entry
    case let .Tree(entry, left, _) where entry.key > key:
        return lookup(key, left)
    case let .Tree(entry, _, right) where entry.key < key:
        return lookup(key, right)
    default:
        fatalError("lookup failed to handle \(records) properly")
    }
}

func insert(_ record: Record, _ records: Records) -> Records {
    switch records {
    case .Empty:
        return makeTree(record, .Empty, .Empty)
    case let .Tree(entry, _, _) where entry.key == record.key:
        return records
    case let .Tree(entry, left, right) where entry.key > record.key:
        return makeTree(entry, insert(record, left), right)
    case let .Tree(entry, left, right) where entry.key < record.key:
        return makeTree(entry, left, insert(record, right))
    default:
        fatalError("insert failed")
    }
}

var myRecords = insert(Record(1,"100"), .Empty)
myRecords = insert(Record(5,"500"), myRecords)
myRecords = insert(Record(2,"200"), myRecords)
myRecords = insert(Record(7,"700"), myRecords)
myRecords = insert(Record(3,"300"), myRecords)
myRecords = insert(Record(9,"900"), myRecords)
print("\(myRecords)")

lookup(2, myRecords)
lookup(6, myRecords)
lookup(9, myRecords)




