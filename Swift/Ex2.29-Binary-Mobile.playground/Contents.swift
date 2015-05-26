import Cocoa

// Exercise 2.29
// A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod of a certain length from which hands either a weight or another binary mobile. We can represent a binary mobile using combound data by constructing it from two branches

class Box<T>{
    let unbox: T
    init(_ value: T) {
        self.unbox = value
    }
}

struct Mobile {
    let left: Branch
    let right: Branch
}

struct Branch {
    let length: Int
    let structure: Structure
}

enum Structure {
    case Weight(Int)
    case Structure(Box<Mobile>)
}

let aMobile = Mobile(left: Branch(length: 3, structure: .Weight(2)),
                    right: Branch(length: 3, structure: .Structure(Box(Mobile(left: Branch(length: 1, structure: .Weight(1)), right: Branch(length: 1, structure: .Weight(1)))))))
// a. Write the corresponding selectors left-branch and right -branch, which return the branches of a mobile, and branch-length and branch-structure, which return components of a branch

let aBranch = aMobile.left
aMobile.right
aBranch.length
aBranch.structure

// b. Using your selectors, define a procedure total-weight that returns the total weight of a mobile

func branchWeight(branch: Branch) -> Int {
    switch branch.structure {
    case let .Weight(weight):
        return weight
    case let .Structure(mobile):
        return totalWeight(mobile.unbox)
    }
}

func totalWeight(mobile: Mobile) -> Int {
    return branchWeight(mobile.left) + branchWeight(mobile.right)
}
totalWeight(aMobile)


// c. A mobile is said to be balanced if the torque applied by its top-left branch is equal to that applied by its top-right branch (that is, if the length of the left rod multiplied by the weight hanging from that rod is equal to the corresponding product for the right side) and if each of the submodules hanging off its branches is balanced. Design a predicate that tests whether a binary mobile is balanced.

func isBalanced(mobile: Mobile) -> Bool {
    let leftBranch = mobile.left
    let rightBranch = mobile.right
    
    let mob = (leftBranch, rightBranch)
    
    switch mob {
    case let (.Weight(leftWeight), .Weight(rightWeight):
        return (leftBranch.length * leftWeight) == (rightBranch.length * rightWeight)
    case let (.Weight(leftWeight), .Structure(rightMobile)):
        return (leftBranch.length * leftWeight) == (rightBranch.length * totalWeight(rightMobile.unbox)) && isBalanced(rightMobile.unbox)
    default:
        return false
    }
}

