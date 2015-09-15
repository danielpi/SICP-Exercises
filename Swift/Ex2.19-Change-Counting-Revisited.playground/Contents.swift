import Cocoa

protocol MathematicsProtocol: Equatable, Comparable, IntegerLiteralConvertible {
    init(_ value: Int)
    init(_ value: Float)
    init(_ value: Double)
    
    func +(lhs: Self, rhs: Self) -> Self
    func -(lhs: Self, rhs: Self) -> Self
    func * (lhs: Self, rhs: Self) -> Self
    func / (lhs: Self, rhs: Self) -> Self
}
extension Int: MathematicsProtocol {}
extension Float: MathematicsProtocol {}
extension Double: MathematicsProtocol {}

// Exercise 2.19
// Consider the change-counting program of section 1.2.2. It would be nice to be able to easily change the currency used by the program, so that we could compute the number of ways to change a British pound, for example. As the program is written, the knowledge of the currency is distributed partly into the procedure first-denomination and partly into the procedure count-change (which knows that there are five kinds of U.S. coins). It would be nicer to be able to supply a list of coins to be used for making change.

// We want to rewrite the procedure cc so that its second argument is a list of the values of the coins to use rather than an integer specifying which coins to use. We could then have lists that defined each kind of currency:

let usCoins:List = [50, 25, 10, 5, 1]
let ukCoins:List = [100, 50, 20, 10, 5, 2, 1, 0.5]
let ausCoins:List = [200, 100, 50, 20, 10, 5]

// We could then call cc as follows
// cc(100, usCoins)

// To do this will require changing the program cc somewhat. It will still have the same form, but it will access its second argument differently, as follows

// Define the procedures firstDenomination, exceptFirstDenomination and isNoMore in terms of primitive operations on list structures. Does the order of the list coinValues affect the answer produced by cc? Why or why not?

func firstDenomination<T>(coinValues: List<T>) -> T {
    return car(coinValues)!
}
func exceptFirstDenomination<T>(coinValues: List<T>) -> List<T> {
    return cdr(coinValues)!
}
func isNoMore<T>(coinValues: List<T>) -> Bool {
    return coinValues.isEmpty()
}

func cc<T: MathematicsProtocol>(amount: T, coinValues: List<T>) -> T {
    switch true {
    case amount == 0:
        return 1
    case (amount < 0):
        return 0
    case isNoMore(coinValues):
        return 0
    default:
        return cc(amount, coinValues: exceptFirstDenomination(coinValues)) + cc((amount - firstDenomination(coinValues)), coinValues: coinValues)
    }
}

//cc(100, usCoins)
cc(100, coinVa