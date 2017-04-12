import Cocoa

// Exercise 1.38
// Leonhard Euler published a continued fraction expansion for e - 2. In this fraction Ni are all 1 and Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8 ... Write a program that uses cont-frac to approximate e.


func kFiniteContFrac(_ N:@escaping (Int)->Double, _ D:@escaping (Int)->Double, _ k:Int) -> Double {
    func contFrac(i:Int) -> Double {
        if i > k {
            return 0.0
        } else {
            return N(i) / (D(i) + contFrac(i: i + 1))
        }
    }
    return contFrac(i: 1)
}

func eulerE() -> Double {
    func N(i:Int) -> Double {
        return 1.0
    }
    func D(i:Int) -> Double {
        if ((i + 1) % 3) == 0 {
            return 2 * (Double(i + 1) / 3)
        } else {
            return 1.0
        }
    }
    return 2 + kFiniteContFrac(N, D, 100)
}
eulerE()

// Used the code below to debug the D() function
func D(_ i:Int) -> Double {
    if ((i + 1) % 3) == 0 {
        return 2 * (Double(i + 1) / 3)
    } else {
        return 1.0
    }
}

var a:[Int] = [1,2,3,4,5,6,7,8,9,10]
let output = a.map{D($0)}
output









