import Cocoa

//: ## Exercise 2.66
//: Implement the lookup procedure for the case where the set of records is structured as a binary tree, ordered by the numerical values of the keys.

class Box<T> {
    let unbox: T
    init(_ value: T) {
        self.unbox = value
    }
}

extension Array {
    var match: (head: T, tail: [T])? {
        return (count > 0) ? (self[0], Array(self[1..<count])) : nil
    }
}

enum TreeSet<T>: Printable {
    case Empty
    case Tree(entry:Box<T>, left:Box<TreeSet<T>>, right: Box<TreeSet<T>>)
    
    var description : String {
        switch self {
        case .Empty:
            return "()"
        case let .Tree(entry, left, right):
            return "(\(entry.unbox) \(left.unbox) \(right.unbox))"
        }
    }
}

func entry<T>(tree: TreeSet<T>) -> T {
    switch tree {
    case let .Tree(entry, left, right):
        return entry.unbox
    default:
        fatalError("Tried to read an entry from an empty tree")
    }
}

func leftBranch<T>(tree: TreeSet<T>) -> TreeSet<T> {
    switch tree {
    case let .Tree(_, left, _):
        return left.unbox
    default:
        fatalError("Tried to read the left branch from an empty tree")
    }
}

func rightBranch<T>(tree: TreeSet<T>) -> TreeSet<T> {
    switch tree {
    case let .Tree(_, _, right):
        return right.unbox
    default:
        fatalError("Tried to read the right branch from an empty tree")
    }
}

func makeTree<T>(entry: T, left:TreeSet<T>, right:TreeSet<T>) -> TreeSet<T> {
    return TreeSet.Tree(entry: Box(entry), left: Box(left), right: Box(right))
}

struct Record: Printable {
    var key: Int
    var value: String
    
    init(_ key: Int, _ value: String) {
        self.key = key
        self.value = value
    }
    
    var description : String {
        return "\(key):\(value)"
    }
}

typealias Records = TreeSet<Record>

func lookup(key: Int, records: Records) -> Record? {
    switch records {
    case .Empty:
        return nil
    case let .Tree(entry, _, _) where entry.unbox.key == key:
        return entry.unbox
    case let .Tree(entry, left, _) where entry.unbox.key > key:
        return lookup(key, left.unbox)
    case let .Tree(entry, _, right) where entry.unbox.key < key:
        return lookup(key, right.unbox)
    default:
        fatalError("lookup failed to handle \(records) properly")
    }
}

func insert(record: Record, records: Records) -> Records {
    switch records {
    case .Empty:
        return makeTree(record, .Empty, .Empty)
    case let .Tree(entry, _, _) where entry.unbox.key == record.key:
        return records
    case let .Tree(entry, left, right) where entry.unbox.key > record.key:
        return makeTree(entry.unbox, insert(record, left.unbox), right.unbox)
    case let .Tree(entry, left, right) where entry.unbox.key < record.key:
        return makeTree(entry.unbox, left.unbox, insert(record, right.unbox))
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
println("\(myRecords)")

lookup(2, myRecords)
lookup(6, myRecords)
lookup(9, myRecords)




