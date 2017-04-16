import Cocoa

// Exercise 2.29
// A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod of a certain length from which hands either a weight or another binary mobile. We can represent a binary mobile using combound data by constructing it from two branches

struct Mobile {
    let left: Branch
    let right: Branch
    
    var weight: Int {
        return self.left.weight + self.right.weight
    }
    
    var isBalanced: Bool {
        return (left.torque == right.torque) && left.structure.isBalanced && right.structure.isBalanced
    }
}

struct Branch {
    let length: Int
    let structure: Structure
    
    init(_ length: Int, _ structure: Structure) {
        self.length = length
        self.structure = structure
    }
    init(length: Int, structure: Structure) {
        self.length = length
        self.structure = structure
    }
    
    var weight: Int {
        switch self.structure {
        case let Structure.Weight(weight):
            return weight
        case let Structure.Structure(mobile):
            return mobile.weight
        }
    }
    
    var torque: Int {
        return self.length * self.weight
    }
}

indirect enum Structure {
    case Weight(Int)
    case Structure(Mobile)
    
    var isBalanced: Bool {
        switch self {
        case .Weight(_):
            return true
        case .Structure(let mobile):
            return mobile.isBalanced
        }
    }
}

let aMobile = Mobile(left: Branch(length: 3, structure: .Weight(2)),
                    right: Branch(length: 3, structure: .Structure(Mobile(left: Branch(length: 2, structure: .Weight(1)), right: Branch(length: 2, structure: .Weight(1))))))

let bMobile = Mobile(left: Branch(3, .Weight(2)),
                    right: Branch(3, .Structure(Mobile(left: Branch(2, .Weight(1)),
                                                       right: Branch(2, .Weight(1))))))
// a. Write the corresponding selectors left-branch and right -branch, which return the branches of a mobile, and branch-length and branch-structure, which return components of a branch

let aBranch = aMobile.left
aMobile.right
aBranch.length
aBranch.structure

// b. Using your selectors, define a procedure total-weight that returns the total weight of a mobile

aMobile.weight

// c. A mobile is said to be balanced if the torque applied by its top-left branch is equal to that applied by its top-right branch (that is, if the length of the left rod multiplied by the weight hanging from that rod is equal to the corresponding product for the right side) and if each of the submodules hanging off its branches is balanced. Design a predicate that tests whether a binary mobile is balanced.

aMobile.isBalanced


// This is a lot different from the ML version. I have done away with the global functions and used Swift properties instead. Not sure if this is a good idea or not. This method does result in the shortest code which I think remains easy to read. The data type definitions are starting to bloat admittedly. I'm not particularly happy with the isBalanced property on the structure but it makes the recursion work after Swift forced me to set the weight and mobiles as different types. The fact that isBalanced could be a very expensive operation, yet it looks like a simple property reference is not ideal (Also you need to understand that it is checking every structure in the whole mobile which is not really implied in the naming).



