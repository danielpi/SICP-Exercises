import Darwin

//: # Exercise 2.75:
//: Implement the constructor make-from-mag-ang in message-passing style. This procedure should be analogous to the make-from-real-imag procedure given above.



typealias ComplexNumberDataObject = (String) -> Double

func square(x: Double) -> Double {
    return x * x
}

func makeFromRealImag(x: Double, _ y: Double) -> ComplexNumberDataObject {
    func dispatch(op: String) -> Double {
        switch op {
        case "real-part":
            return x
        case "imag-part":
            return y
        case "magnitude":
            return pow(square(x) + square(y), 0.5)
        case "angle":
            return atan2(y, x)
        default:
            fatalError("Unknown operations: \(op)")
        }
    }
    return dispatch
}

func applyGeneric(op: String, _ arg: ComplexNumberDataObject) -> Double {
    return arg(op)
}

let a = makeFromRealImag(3, 4)
applyGeneric("magnitude", a)
applyGeneric("angle", a)


func makeFromMagAng(r: Double, _ A: Double) -> ComplexNumberDataObject {
    func dispatch(op: String) -> Double {
        switch op {
        case "real-part":
            return r * cos(A)
        case "imag-part":
            return r * sin(A)
        case "magnitude":
            return r
        case "angle":
            return A
        default:
            fatalError("Unknown operation: \(op)")
        }
    }
    return dispatch
}

let b = makeFromMagAng(5, 0.927295218)
applyGeneric("real-part", b)
applyGeneric("imag-part", b)



