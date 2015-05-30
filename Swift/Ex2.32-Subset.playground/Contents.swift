import Cocoa

// Exercise 2.32
// We can represent a set as a list of distinct elements, and we can represent the set of all subsets of the set as a list of lists. For example, if the set is (1,2,3), then the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following definition of a procedure that generates the set of subsets of a set and give a clear explanation of why it works

/*
(define (subsets s)
    (if (null? s)
        (list nil)
        (let ((rest (subsets (cdr s))))
            (append rest (map <??> rest)))))

*/


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


func subsets(s: [Int]) -> [[Int]] {
    if s.isEmpty {
        return [[]]
    } else {
        let rest = subsets(cdr(s))
        return rest + map(rest) { cons(car(s), $0) }
    }
}

subsets([1,2,3])
subsets([1,2,3,4])




