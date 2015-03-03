import Cocoa

// Exercise 1.37
// An infinite continued fraction is an expression of the form
//
//            N1
// f = ------------------
//     D1 +      N2
//          -------------
//          D2 +    N3
//               --------
//               D3 + ...
//
// As an example, the inifinite continued fraction expansion with the Ni and Di all equal to 1 produces 1 / Golden Ratio.
//
// One way to approximate an infinite continued fraction is to truncate the expansion after a given number of terms. Such a truncation -- a so-called k-term finitie continued fraction -- has the form
//
//            N1
// f = ------------------
//     D1 +      N2
//          -------------
//          D2 +    NK
//               --------
//                  DK
//
// Suppose that n and d are procedures of one argument (the term i) that return the Ni and Di of the terms of the continued fraction. Define a procedure cont-frac such that evaluating (cont-frac n d k) computes the value of the k-term finite continued fraction. Check your procedure by approximating 1 / Golden ratio

func kFiniteContFrac(N:(Int)->Double, D:(Int)->Double, k:Int) -> Double {
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

func goldenRation() -> Double {
    func N(i:Int) -> Double {
        return 1.0
    }
    func D(i:Int) -> Double {
        return 1.0
    }
    
    return 1 / kFiniteContFrac( N, D, 100)
}
goldenRation()

// How large must k be in order to get an approximation that is accurate to 4 decimal places?
// k needs to be 13 or higher


// b) Write an iterative version of cont-frac
func kFiniteContFracIter(N:(Int)->Double, D:(Int)->Double, k:Int) -> Double {
    var contFrac: (i: Int, output:Double) -> Double = { _ in return 0.0 }
    contFrac = { i, output in
        if i == 0 {
            return output
        } else {
            return contFrac(i: i - 1, output: N(i) / (D(i) + output))
        }
    }
    return contFrac(i: k, output: 0.0)
}

func goldenRationIter() -> Double {
    func N(i:Int) -> Double {
        return 1.0
    }
    func D(i:Int) -> Double {
        return 1.0
    }
    
    return 1 / kFiniteContFracIter( N, D, 100)
}
goldenRationIter()


