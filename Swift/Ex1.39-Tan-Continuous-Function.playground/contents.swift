import Cocoa

// Exercise 1.39
// A continued fraction representation of the tangent function was published in 1770 by German mathematician J.H.Lambert

/*
              x
tan x = ---------------
        1 -     x^2
            -----------
            3 -   x^2
                -------
                5 - ...

*/

// where x is in radians. Define a procedure tand-cf(x k) that computes an approximation to the tangent function based on Lambert's formula.

func kFiniteContFrac(N:(Int)->Double, _ D:(Int)->Double, _ k:Int) -> Double {
    var contFrac: (i:Int) -> Double = { _ in return 0.0 }
    contFrac = { i in
        if i > k {
            return 0.0
        } else {
            return N(i) / (D(i) + contFrac(i: i + 1))
        }
    }
    return contFrac(i: 1)
}

func tanCF(x: Double, _ k: Int) -> Double {
    func N(i:Int) -> Double {
        if i == 1 {
            return -1 * x
        } else {
            return -1 * x * x
        }
    }
    func D(i:Int) -> Double {
        return Double((i * 2) - 1)
    }
    
    return -1 * kFiniteContFrac(N, D, k)
}

tanCF(1.0, 10)
tan(1.0)

