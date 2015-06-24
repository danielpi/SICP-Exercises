import Cocoa

//: ## 2.3 Symbolic Data
//: All the compound data objects we have used so far were constructed ultimately from numbers. In this section we extend the representaional capability of our language by introducing the ability to work with arbitrary symbols as data.

//: ### 2.3.1 Quotation
//: If we can form compound data using symbols, we can have lists such as 

// [a, b, c, d]
[23, 45, 17]
["Norah": 12, "Molly": 9, "Anna": 7, "Lauren": 6, "Charlotte": 4]

//: Lists containing symbols can look just like the expressions of our language:

// (23 + 45) * ( x + 9)
func fact(n: Int) -> Int {
    if n == 1 {
        return 1
    } else {
        return n * fact(n - 1)
    }
}
fact(4)

//: In order to manipulate symbols we need a new element in our language: the ability to *quote* a data object. Suppose we want to construct the list (a b).
