import Cocoa

// Exercise 1.32
// a) Show that sum and product are both special cases of a still more general notion called accumulate that combines a collection of terms, using some general function

// func accumulate(combiner:,nullValue:, term: ,a:, next: , b:) ->

// Write accumulate and show how sum and product can both be defined as simple calls to accumulate
func accumulateDouble(combiner: (Double, Double) -> Double,
                     nullValue: Double,
                          term: (Int) -> Double,
                             a: Int,
                          next: (Int) -> Int,
                             b: Int) -> Double {
        if (a > b) {
            return nullValue
        } else {
            return combiner(term(a), accumulateDouble(combiner, nullValue, term, next(a), next, b))
        }
}

// Implementing sum
func sumDouble(term:(Int) -> Double, a: Int, next: (Int) -> Int, b: Int) -> Double {
    return accumulateDouble(+, 0.0, term, a, next, b)
}


func inc(n: Int) -> Int {
    return n + 1
}
func isEven(n: Int) -> Bool {
    return (n % 2) == 0
}

func integral(f:(Double) -> Double, a:Double, b:Double, n:Int) -> Double {
    func h(a: Double, b: Double, n: Int) -> Double {
        return (b - a) / Double(n)
    }
    func yk(f:(Double) -> Double, a: Double, b: Double, n:Int, k:Int) -> Double {
        let x = a + (Double(k) * h(a,b,n))
        return f(x)
    }
    func simpsonsTerm(k:Int) -> Double {
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

func cube(x: Double) -> Double {
    return x * x * x
}
func identity<T>(x: T) -> T {
    return x
}
integral(cube, 0, 1, 100)
integral(identity, 0, 1, 100)


// Implementing Product
func productDouble(term:(Int) -> Double, a: Int, next: (Int) -> Int, b: Int) -> Double {
    return accumulateDouble(*, 1.0, term, a, next, b)
}


func DRPpi(steps: Int) -> Double {
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
func accumulateIter(combiner: (Double, Double) -> Double, term: (Int) -> Double, a: Int, next: (Int) -> Int, b: Int, result: Double) -> Double {
    if (a > b) {
        return result
    } else {
        return accumulateIter(combiner, term, next(a), next, b, combiner(result, term(a)))
    }
}

func accumulate2(combiner: (Double, Double) -> Double, nullValue: Double, term: (Int) -> Double, a: Int, next: (Int) -> Int,b: Int) -> Double {
    
    return accumulateIter(combiner, term, a, next, b, nullValue)
}



// Implementing sum
func sum(term:(Int) -> Double, a: Int, next: (Int) -> Int, b: Int) -> Double {
    return accumulate2(+, 0.0, term, a, next, b)
}


func integral2(f:(Double) -> Double, a:Double, b:Double, n:Int) -> Double {
    func h(a: Double, b: Double, n: Int) -> Double {
        return (b - a) / Double(n)
    }
    func yk(f:(Double) -> Double, a: Double, b: Double, n:Int, k:Int) -> Double {
        let x = a + (Double(k) * h(a,b,n))
        return f(x)
    }
    func simpsonsTerm(k:Int) -> Double {
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

integral2(cube, 0, 1, 100)
integral2(identity, 0, 1, 100)


// Implementing Product
func product(term:(Int) -> Double, a: Int, next: (Int) -> Int, b: Int) -> Double {
    return accumulate2(*, 1.0, term, a, next, b)
}


func DRPpi2(steps: Int) -> Double {
    func term(k: Int) -> Double {
        if isEven(k) {
            return (Double(k) + 2) / (Double(k) + 1)
        } else {
            return (Double(k) + 1) / (Double(k) + 2)
        }
    }
    return 4 * productDouble(term, 1, inc, steps)
}

DRPpi2(100)












