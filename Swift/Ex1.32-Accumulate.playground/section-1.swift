import Cocoa

// Exercise 1.32
// a) Show that sum and product are both special cases of a still more general notion called accumulate that combines a collection of terms, using some general function

// func accumulate(combiner:,nullValue:, term: ,a:, next: , b:) ->

// Write accumulate and show how sum and product can both be defined as simple calls to accumulate
func accumulateDouble(_ combiner: (Double, Double) -> Double,
                   _ nullValue: Double,
                        _ term: (Int) -> Double,
                           _ a: Int,
                        _ next: (Int) -> Int,
                           _ b: Int) -> Double {
        if (a > b) {
            return nullValue
        } else {
            return combiner(term(a), accumulateDouble(combiner, nullValue, term, next(a), next, b))
        }
}

// Implementing sum
func sumDouble(_ term:(Int) -> Double, _ a: Int, _ next: (Int) -> Int, _ b: Int) -> Double {
    return accumulateDouble(+, 0.0, term, a, next, b)
}


func inc(_ n: Int) -> Int {
    return n + 1
}
func isEven(_ n: Int) -> Bool {
    return (n % 2) == 0
}

func integral(_ f:@escaping (Double) -> Double, a:Double, b:Double, n:Int) -> Double {
    func h(_ a: Double, _ b: Double, _ n: Int) -> Double {
        return (b - a) / Double(n)
    }
    func yk(_ f:(Double) -> Double, _ a: Double, _ b: Double, _ n:Int, _ k:Int) -> Double {
        let x = a + (Double(k) * h(a,b,n))
        return f(x)
    }
    func simpsonsTerm(_ k:Int) -> Double {
        switch true {
        case k == 0 || k == n:
            return yk(f, a, b, n, k)
        case isEven(k):
            return 2 * yk(f, a, b, n, k)
        default:
            return 4 * yk(f, a, b, n, k)
        }
    }
    return (h(a,b,n) / 3.0) * sumDouble(simpsonsTerm, 0, inc, n)
}

func cube(_ x: Double) -> Double {
    return x * x * x
}
func identity<T>(x: T) -> T {
    return x
}
integral(cube, a:0, b:1, n:100)
integral(identity, a:0, b:1, n:100)


// Implementing Product
func productDouble(_ term:(Int) -> Double, _ a: Int, _ next: (Int) -> Int, _ b: Int) -> Double {
    return accumulateDouble(*, 1.0, term, a, next, b)
}


func DRPpi(_ steps: Int) -> Double {
    func term(k: Int) -> Double {
        if isEven(k) {
            return (Double(k) + 2) / (Double(k) + 1)
        } else {
            return (Double(k) + 1) / (Double(k) + 2)
        }
    }
    return 4 * productDouble(term, 1, inc, steps)
}

DRPpi(100)



// b) Write accumulate again as an iterative process
func accumulateIter(_ combiner: (Double, Double) -> Double, _ term: (Int) -> Double, _ a: Int, _ next: (Int) -> Int, _ b: Int, _ result: Double) -> Double {
    if (a > b) {
        return result
    } else {
        return accumulateIter(combiner, term, next(a), next, b, combiner(result, term(a)))
    }
}

func accumulate2(_ combiner: (Double, Double) -> Double, _ nullValue: Double, _ term: (Int) -> Double, _ a: Int, _ next: (Int) -> Int, _ b: Int) -> Double {
    
    return accumulateIter(combiner, term, a, next, b, nullValue)
}



// Implementing sum
func sum(_ term:(Int) -> Double, _ a: Int, _ next: (Int) -> Int, _ b: Int) -> Double {
    return accumulate2(+, 0.0, term, a, next, b)
}


func integral2(_ f:@escaping (Double) -> Double, a:Double, b:Double, n:Int) -> Double {
    func h(_ a: Double, _ b: Double, _ n: Int) -> Double {
        return (b - a) / Double(n)
    }
    func yk(_ f:(Double) -> Double, _ a: Double, _ b: Double, _ n:Int, _ k:Int) -> Double {
        let x = a + (Double(k) * h(a,b,n))
        return f(x)
    }
    func simpsonsTerm(_ k: Int) -> Double {
        switch true {
        case k == 0 || k == n:
            return yk(f, a, b, n, k)
        case isEven(k):
            return 2 * yk(f, a, b, n, k)
        default:
            return 4 * yk(f, a, b, n, k)
        }
    }
    return (h(a,b,n) / 3.0) * sum(simpsonsTerm, 0, inc, n)
}

integral2(cube, a:0, b:1, n:100)
integral2(identity, a: 0,b: 1, n:100)


// Implementing Product
func product(_ term:(Int) -> Double, _ a: Int, _ next: (Int) -> Int, _ b: Int) -> Double {
    return accumulate2(*, 1.0, term, a, next, b)
}


func DRPpi2(_ steps: Int) -> Double {
    func term(k: Int) -> Double {
        if isEven(k) {
            return (Double(k) + 2) / (Double(k) + 1)
        } else {
            return (Double(k) + 1) / (Double(k) + 2)
        }
    }
    return 4 * productDouble(term, 1, inc, steps)
}

DRPpi2(1000)












