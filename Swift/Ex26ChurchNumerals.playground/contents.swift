import Cocoa

// Exercise 2.6
// In case representing pairs as procedures wasn't mind-boggling enough, consider that, in a language that can manipulate procedures, we can get by without numbers (at least insofar as nonnegative integers are concerned) by implementing 0 and the operation of adding 1 as

typealias DRPFunction = (Int) -> Int
func zero() -> DRPFunction {
    return { x in return x }
}

func addOne(n: DRPFunction) ->