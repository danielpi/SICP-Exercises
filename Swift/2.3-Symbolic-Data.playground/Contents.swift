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

//: In order to manipulate symbols we need a new element in our language: the ability to *quote* a data object. Suppose we want to construct the list (a b). We can't accomplish this with (list a b), because this expression constructs a list of the *values* of a and b rather than the symbols themselves. This issue is well known in the context of natural languages, where words and sentences may be regarded either as semantic entities or as character strings (syntactic entities). The common practice in natrual languages is to use quotation marks to indicate that a word or a sentence is to be treated literally as a string of characters. For instance, the first letter of "John" is clearly "J". If we tell somebody "say your name aloud," we expect to hear that person's name. However, if we tell somebody "say 'your name' aloud," we expect to hear the words "your name." Note that we are forced to nest quotation marks. to describe what somebody else might say.

//: We can follow this same practice to identify lists and symbols that are to be treated as data objects rather than as expressions to be evaluated. However, our formate for quoting differs from that of natural languages in that we place a quotation mark (traditionally, the single quote symbol ') only at the beginning of the object to be quoted. We can get away with this in Scheme syntax because we rely on blanks and parentheses to delimit objects. Thus, the meaning of the single quote character is to quote the next object.

//: Now we can distinguish between symbols and their values:

// I'm not too sure that there is an equivelent to Schemes symbols in Swift. One suggestion is that they are like enums that don't need to be defined. Kind of klunky
enum General: Int {
    case a = 1
    case b = 2
}

[General.a.rawValue, General.b.rawValue]
[General.a, General.b]
//[&a, b]

// Symbols are sort of like dictionary keys as well. Doesn't work out practically though.
var symbols = ["a": 1, "b": 2]
[symbols["a"], symbols["b"]]
Array(symbols.keys)[0]
Array(symbols.keys)[1]

// Maybe we can make our own symbol type. When I do this has some good information http://www.phyast.pitt.edu/~micheles/scheme/scheme7.html
/*
struct Symbol<T> {
    let string: String
    var value: T?
}

func quote(name: String) -> Symbol {
    return Symbol(string: name)
}

func hash(symbol: Symbol) -> Int {

}

func eq?(lhs: Symbol, rhs: Symbol) -> Bool {

}

prefix func ' (name: String) -> Symbol {
    return quote(name)
}
let a =
*/

//: Quotation also allows us to type in combound objects, using the conventional printed representation for lists:

// (car '(a b c))
// (cdr '(a b c))
// I've got nothing here. Any suggestions for how to do this in Swift are welcome.

//: In keeping with this, we can obtain the empty list by evaluating '(), and thus dispense with the variable nil.

//: One additional primitive used in manipulating symbols is eq?, which takes two symbols as arguments and tests whether they are the same. Using eq?, we can implement a useful procedure called memq. This takes two arguments, a symbol and a list. If the symbol is not contained in the list (i.e., is not eq? to any item in the list), then memq returns false. Otherwise, it returns the sublist of the list beginning with the first occurence of the symbol:

func memq<T: Equatable>(item: T, list: [T]) -> [T]? {
    switch true {
        case list.isEmpty:
            return nil
        case item == list[0]:
            return list
        default:
            return memq(item, Array(list[1..<list.count]))
    }
}

// For example, the value of 

memq("apple", ["pear","banana","prune"])

// is nil, whereas the value of

memq("apple", ["x", "apple sauce", "y", "apple", "pear"])

// is ["apple", "pear"]




