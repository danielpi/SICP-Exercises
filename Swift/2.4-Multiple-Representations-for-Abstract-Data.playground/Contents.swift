import Cocoa
//: ## 2.4 Multiple Representations for Abstract Data
//: We have introduced data abstraction, a methodology for structuring systems in such a way that much of a program can be specified independent of the choices involved in implementing the data objects that the program manipulates. For example, we saw in section 2.1.1 how to separate the task of designing a program that uses rational numbers from the task of implementing rational numbers in terms of the computer language's primitive mechanisms for constructing compound data. The key idea was to erect an abstraction barrier -- in this case, the selectors and constructors for rational numbers (make-rat, numer, denom) -- that isolates the way rational numbers are used from their underlying representation in terms of list structure. A similar abstraction barrier isolates the details of the procedures that perform rational arithmetic (add-rat, sub-rat, mul-rat, and div-rat) from the "higher-level" procedures that use rational numbers. The resulting program has the structure shown in figure 2.1
//:
//: These data-abstraction barriers are powerful tools for controlling complesxity. By isolating the underlying representations of data objects, we can divide the task of designing a large program into smaller tasks that can be performed separately. But this kind of data abstraction is not yet powerful enough, because it may not always make sense to speak of "the underlying representation" for a data object.
//:
//: For one thing, there might be more than one useful representation for a data object, and we might like to design systems that can deal with multiple representations. To take a simple example, complex numbers may be represented in two almost equivalent ways: in rectangular form (real and imaginary parts) and in polar form (magnitude and angle). Sometimes rectangular form is more appropriate and sometimes polar form is more appropriate. Indeed, it is perfectly plausible to imagine a system in which complex numbers are represented in both ways, and in which the procedures for manipulating complex numbers work with either representation.
//:
//: More importantly, programming systems are often designed by many people working over extended periods of time, subject to requirements that change over time. In such an environment, it is simply not possible for everyone to agree in advance on choices of data representation. So in addition to the data-abstraction barriers that isolate repreentation from use, we need abstraction barriers that isolate different design choices from each other and permit different choices to coexist in a single program. Furthermore, since large programs are often created by combining pre-existing modules that were designed in isolation, we need conventions that permit programmers to incorporate modules that were designed in isolation, we need conventions that permit programmers to incorporate modules into larger systems *additively*, that is, without having to redesign or reimplement these modules.
//: 
//: In this section, we will learn how to cope with data that may be represented in different ways by different parts of a program. This requires constructing generic procedures -- procedures that can operate on data that may be represented in more than one way. Our main technique for building generic procedures will be to work in terms of data objects that have type tags, that is, data objects that include explicit information about how they are to be processed. We will also discuss data-directed programming, a powerful and convenient implementation strategy for additively assembling systems with generic operations. 
//:
//: We begin with the simple complex-number example. We will see how type tags and data-directed style enable us to design separate rectangular and polar representations for complex numbers while maintaining the notion of an abstract "complex-number" data object. We will accomplish this by defining arithmetic procedures for complex numbers (add-complex, sub-complex, mul-complex, and div-complex) in terms of generic selectors that access parts of a complex number independent of how the number is represented. The resulting complex-number system, as shown in figure 2.19, contains two different kinds of abstraction barriers. The "horizontal" abstraction barriers play the same role as the ones in figure 2.1. They isolate "higher-level" operations from "lower-level" representationis. In addition, there is a "vertical" barrier that gives us the ability to separately design and install alternative representations.
//:
//:                  Programs that use complex numbers
//:         ---------------------------------------------------
//:      ---| add-complex sub-complex mul-complex div-complex |---
//:         ---------------------------------------------------
//:                    Complex-arithmetic package
//:      ---------------------------------------------------------
//:              Rectangular        |           Polar
//:            representation       |       representation
//:      ---------------------------------------------------------
//:           List structure and primitive machine arithmetic
//:
//: **Figure 2.19:** Data-abstraction barriers in the complex-number system.
//:
//: In section 2.5 we will show how to use type tags and data-directed style to develop a generic arithmetic package. This provides procedures (add, mul, and so on) that can be used to manipulate all sorts of "numbers" and can be easily extended when a new kind of number is needed. In section 2.5.3, we'll show how to use generic arithmetic in a system that performs symbolic algebra.
//:
//:
//: ### 2.4.1 Representations for Complex Numbers
//: We will develop a system that performs arithmetic operations on complex numbers as a simple but unrealistic example of a program that uses generic operations. We begin by discussing two plausible representations for complex numbers as ordered pairs: rectangular form (real part and imaginary part) and polar form (magnitude and angle). Section 2.4.2 will show how both representations can be made to coexist in a single system through the use of type tags and generic operations.
//:
//: Like rational numbers, complex numbers are naturally represented as ordered pairs. The set of complex numbers can be thought of as a two-dimensional space with two orthogonal axes, the "real" axis and the "imaginary" axis. (See figure 2.20). 
//:
//:     Imaginary
//:         ^
//:         |
//:         |
//:       y |--------+ z = x + iy = re^iA
//:         |       /|
//:         |      / |
//:         |     /  |
//:         |  r /   |
//:         |   /    |
//:         |  /     |
//:         | /      |
//:         |/ A     |
//:       --+--------------->   Real
//:         |        x
//:    
//:     Figure 2.20: Complex numbers as points in the plane
//: From this point of view, the complex number z = x + iy (where i^2 = -1) can be thought of as the point in the plane whose real coordinate is x and whose imaginary coordinate is y. Addition of complex numbers reduces in this representation to addition of coordinates:
//:
//:         Real-part(z1 + z2) = Real-part(z1) + Real-part(z2),
//:    Imaginary-part(z1 + z2) = Imaginary-part(z1) + Imaginary-part(z2).
//:
//: When multiplying complex numbers, it is more natural to think in terms of representing a complex number in polar form, as a magnitude and an angle (r and A in Figure 2.20). The product of two complex numbers is the vector obtained by stretching one complex number by the length of the other and then rotating it through the angle of the other:
//:
//:     Magnitude(z1 . z2) = Magnitude(z1) . Magnitude(z2),
//:         Angle(z1 . z2) = Angle(z1) + Angle(z2).
//:
//: Thus, there are two different representations for complex numbers, which are appropriate for different operations. Yet, from the viewpoint of someone writing a program that uses complex numbers, the principle of data abstraction suggests that all the operations for manipulating complex numbers should be available regardless of which representation is used by the computer. For example, it is often useful to be able to find the magnitude of a complex number that is specified by rectangular coordinates. Similarly, it is often useful to be able to determine the real part of a complex number that is specified by polar coordinates.
//:
//: To design such a system, we can follow the same data-abstraction strategy we followed in designing the rational-number package in Section 2.1.1. Assume that the operations on complex numbers are implemented in terms of four selectors: real-part, imag-part, magnitude and angle. Also assume that we have two procedures for constructing complex numbers: make-from-real-imag returns a complex number with specified real and imaginary parts, and make-from-mag-ang returns a complex number with specified magnitude and angle. These procedures have the property that, for any complex number z, both 
//:
//:     makeFromRealImag(realPart(z), imagPart(z))
//:
//: and
//: 
//:    makeFromMagAng(magnitude(z), angle(z))
//:
//: produce complex numbers that are equal to z.
//:
//: Using these constructors and selectors, we can implement arithmetic on complex numbers using the "abstract data" specified by the constructors and selectors, just as we did for rational numbers in Section 2.11. As shown in the formulas above, we can add and subtract complex numbers in terms of real and imaginary parts while multiplying and dividing complex numbers in terms of magnitudes and angles:

func square(x: Double) -> Double { return x * x }
//func sqrt(x: Double) -> Double { return pow(x, 0.5) }

typealias RectangularForm = (Double, Double)
func realPart1(z: RectangularForm) -> Double { return z.0 }
func imagPart1(z: RectangularForm) -> Double { return z.1 }
func magnitude1(z: RectangularForm) -> Double { return sqrt(square(realPart1(z)) + square(imagPart1(z))) }
func angle1(z: RectangularForm) -> Double { return atan2(imagPart1(z), realPart1(z)) }
func makeFromRealImag1(x: Double, y: Double) -> RectangularForm { return (x, y) }
func makeFromMagAng1(r: Double, a: Double) -> RectangularForm { return (r * cos(a), r * sin(a)) }

let a = makeFromRealImag1(3, y: 4)
realPart1(a)
imagPart1(a)
magnitude1(a)
angle1(a)

let b = makeFromMagAng1(1, a: 3.14 / 4)
realPart1(b)
imagPart1(b)
magnitude1(b)
angle1(b)

func addComplex1(z1: RectangularForm, z2: RectangularForm) -> RectangularForm {
    return makeFromRealImag1(realPart1(z1) + realPart1(z2), y: imagPart1(z1) + imagPart1(z2))
}
func subComplex1(z1: RectangularForm, z2: RectangularForm) -> RectangularForm {
    return makeFromRealImag1(realPart1(z1) - realPart1(z2), y: imagPart1(z1) - imagPart1(z2))
}
func mulComplex1(z1: RectangularForm, z2: RectangularForm) -> RectangularForm {
    return makeFromMagAng1(magnitude1(z1) * magnitude1(z2), a: angle1(z1) + angle1(z2))
}
func divComplex1(z1: RectangularForm, z2: RectangularForm) -> RectangularForm {
    return makeFromMagAng1(magnitude1(z1) / magnitude1(z2), a: angle1(z1) - angle1(z2))
}

addComplex1(a, b)
subComplex1(a, b)
mulComplex1(a, b)
divComplex1(a, b)

//: To complete the complex-number package, we must choose a representations and we must implement the constructors and selectors in terms of primitive numbers and primitive list structure. There are two obvious ways to do this: we can represent a complex number in "rectangular form" as a pair (real part, imaginary part) or in "polar form" as a pair (magnitude, angle). Which shall we choose?
//:
//: In order to make the different choices concrete, imagine that there are two programmers, Ben Bitdiddle and Alyssa P. Hacker, who are independently designing representations for the complex-number system. Ben chooses to represent complex numbers in rectangular form. With this choice, selecting the real and imaginary parts of a complex number is straightforward, as is constructing a omplex number with given real and imaginary parts. To find the magnitude and the angle, or to construct a complex number with a given magnitude and angle, he uses the trigonometric relations.
//:
//:    x = r cos A,  r = (x^2 + y^2)^0.5
//:    y = r sin A,  A = arctan(y,x)
//:
//: which relate the real and imaginary parts (x,y) to the magnitude and the angle (r,A). Ben's representation is therefore given by the following selectors and constructors.
//:
//: See above
//:
//: Alyssa, in contrast, chooses to represent complex numbers in polar form. For her, selecting the magnitude and angle is straighforward, but she has to use the trigonometric relations to obtain the real and imaginary parts. Alyssa's representation is:

typealias PolarForm = (Double, Double)
func realPart2(z: PolarForm) -> Double {
    return magnitude2(z) * cos(angle2(z))
}
func imagPart2(z: PolarForm) -> Double {
    return magnitude2(z) * sin(angle2(z))
}
func magnitude2(z: PolarForm) -> Double { return z.0 }
func angle2(z: PolarForm) -> Double { return z.1 }
func makeFromRealImag2(x: Double, y: Double) -> PolarForm {
    return PolarForm(sqrt(square(x) + square(y)), atan2(y, x))
}
func makeFromMagAng2(r: Double, A: Double) -> PolarForm {
    return PolarForm(r, A)
}

let polarA = makeFromRealImag2(6, y: 8)
let polarB = makeFromMagAng2(2, A: 0.5)
realPart2(polarA)
imagPart2(polarA)
magnitude2(polarB)
angle2(polarB)

//: The discipline of data abstraction ensures that the same implementation of add-complex, sub-complex, mul-complex, and div-complex will work with either Ben's representation or Alyssa's representation.
//:
//:
//: ## 2.4.2 Tagged data
//:
//: One way to view data abstraction is as an application of the "principle of least commitment." In implementing the complex-number system in Section 2.4.11, we can use either Ben's rectangular representation or Alyssa's polar representation. The abstraction barrier formed by the selectors and constructors permits us to defer to the  last possible moment the choice of a concrete representation for our data objects and thus retain maximum flexibility in our system design.
//:
//: The principle of least commitment can be carried to even further extremes. If we desire, we can maintain the ambiquity of representation even after we have designed the selectors and constructors, and elect to use both Ben's representation *and* Alyssa's representation. If both representations are included in a single system, however, we will need some way to distinquish data in polar form from data in rectangular form. Otherwise, if we were asked, for instance, to find the magnitude of the pair (3,4), we wouldn't know whether to answer 5 (interpreting the number in rectangular form) or 3 (interpreting the number in polar form). A straightforward way to accomplish this distinction is to include a type tag - the symbol rectangle or polar - as part of each complex number. Then when we need to manipulate a complex number we can use the tag to decide which selector to apply.
//:
//: In order to manipulate tagged data, we will assume that we have procedures type-tag and contents that extract from a data object the tag and the actual contents (the polar or rectangular coordinates, in the case of a complex number). We will also postulate a procedure attach-tag that takes a tag and contents and produces a tagged data object. A straightforward way to implement this is to use ordinary list structure:


enum ComplexNumberType { case Rectangular, Polar }
typealias ComplexNumber = (Double, Double)
typealias TaggedComplexNumber = (ComplexNumberType, ComplexNumber)

func attachTag(typeTag: ComplexNumberType, contents: ComplexNumber) -> TaggedComplexNumber {
    return (typeTag, (contents.0, contents.1))
}
func typeTag(datum: TaggedComplexNumber) -> ComplexNumberType {
    return datum.0
}
func contents(datum: TaggedComplexNumber) -> ComplexNumber {
    return datum.1
}

//: Using these procedures, we can define predicates rectangular? and polar?, which recognize rectangular and polar numbers, respectively:

func isRectangular(z: TaggedComplexNumber) -> Bool {
    return z.0 == .Rectangular
}
func isPolar(z: TaggedComplexNumber) -> Bool {
    return z.0 == .Polar
}

//: With type tags, Ben and Alyssa can now modify their code so that their two different representations can coexist in the same system. Whenever Ben constructs a complex number, he tags it as rectangular. Whenever Alyssa constructs a complex number, she tags it as polar. In addition, Ben and Alyssa must make sure that the names of their procedures do not conflict. One way to do this is for Ben to append the suffix rectangular to the name of each of his representation procedures and for Alyssa to append polar to the names of hers. Here is Ben's revised rectangular representation from Section 2.4.1:

func realPartRectangular(z: ComplexNumber) -> Double { return z.0 }
func imagPartRectangular(z: ComplexNumber) -> Double { return z.1 }
func magnitudeRectangular(z: ComplexNumber) -> Double {
    return pow(square(realPartRectangular(z)) + square(imagPartRectangular(z)), 0.5)
}
func angleRectangular(z: ComplexNumber) -> Double {
    return atan2(imagPartRectangular(z), realPartRectangular(z))
}
func makeFromRealImagRectangular(x: Double, y: Double) -> TaggedComplexNumber {
    return attachTag(.Rectangular, contents: (x, y))
}
func makeFromMagAngRectangular(r: Double, A: Double) -> TaggedComplexNumber {
    return attachTag(.Rectangular, contents: (r * cos(A), r * sin(A)))
}

let rectangularDatumA = makeFromRealImagRectangular(7, y: 13)
let rectangularDatumB = makeFromMagAngRectangular(10,A: -2)

//: and here is Alyssa's revised polar representation:

func realPartPolar(z: ComplexNumber) -> Double {
    return magnitudePolar(z) * cos(anglePolar(z))
}
func imagPartPolar(z: ComplexNumber) -> Double {
    return magnitudePolar(z) * sin(anglePolar(z))
}
func magnitudePolar(z: ComplexNumber) -> Double { return z.0 }
func anglePolar(z: ComplexNumber) -> Double { return z.1 }
func makeFromRealImagPolar(x: Double, y: Double) -> TaggedComplexNumber {
    let tag: ComplexNumberType = .Polar
    let r = pow(square(x) + square(y), 0.5)
    let A = atan2(y, x)
    return attachTag(tag, contents: (r, A))
}
func makeFromMagAngPolar(r: Double, A: Double) -> TaggedComplexNumber {
    return attachTag(.Polar, contents: (r, A))
}

let polarDatumA = makeFromRealImagPolar(4, y: 9)
let polarDatumB = makeFromMagAngPolar(6, A: 0.8)

//: Each generic selector is implemented as a procedure that checks the tag of its argument and calls the appropriate procedure for handling data of that type. For example, to obtain the real part of a complex number, real-part examines the tag to determine whether to use Ben's real-part-rectangular or Alyssa's real-part-polar. In either case, we use contents to extract the bare, untagged datum and send this to the rectangular or polar procedure as required.

func realPart3(z: TaggedComplexNumber) -> Double {
    switch typeTag(z) {
    case .Rectangular:
        return realPartRectangular(contents(z))
    case .Polar:
        return realPartPolar(contents(z))
    }
}
func imagPart3(z: TaggedComplexNumber) -> Double {
    switch typeTag(z) {
    case .Rectangular:
        return imagPartRectangular(contents(z))
    case .Polar:
        return imagPartPolar(contents(z))
    }
}
func magnitude3(z: TaggedComplexNumber) -> Double {
    switch typeTag(z) {
    case .Rectangular:
        return magnitudeRectangular(contents(z))
    case .Polar:
        return magnitudePolar(contents(z))
    }
}
func angle3(z: TaggedComplexNumber) -> Double {
    switch typeTag(z) {
    case .Rectangular:
        return angleRectangular(contents(z))
    case .Polar:
        return anglePolar(contents(z))
    }
}

// Note that in Swift we can do away with the error condition associated with an unhandled data type because we use an Enum and the compiler enforces us to handle all possible cases in the switch statements.

//: To implement the complex-number arithmetic operations, we can use the same procedures add-complex, sub-complex, mul-complex, and div-complex from Section 2.4.1, because the selectors they call are generic, and so will work with either representation. For example, the procedure add-complex is still

func addComplex(z1: TaggedComplexNumber, z2: TaggedComplexNumber) -> TaggedComplexNumber {
    return makeFromRealImag3(realPart3(z1) + realPart3(z2), y: imagPart3(z1) + imagPart3(z2))
}

//: Finally, we must choose whether to construct complex numbers using Ben's representation or Alyssa's representation. One reasonable choice is to construct rectangular numbers whenever we have real and imaginary parts and to construct polar numbers whenever we have magnitudes and angles:

func makeFromRealImag3(x: Double, y: Double) -> TaggedComplexNumber {
    return makeFromRealImagRectangular(x, y: y)
}
func makeFromMagAng3(r: Double, A: Double) -> TaggedComplexNumber {
    return makeFromMagAngPolar(r, A: A)
}

let AAA = makeFromRealImag3(3, y: 4)
let BBB = makeFromMagAng3(5, A: 0.2)
let CCC = addComplex(AAA, BBB)

//:                    Programs that use complex numbers
//:          ------------------------------------------------------
//:     ----|  add-complex  sub-complex  mul-complex  div-complex  |----
//:          ------------------------------------------------------
//:                        Complex-arithmetic package
//:                        --------------------------
//:     __________________|  real-part    magnitude  |__________________
//:                       |  imag-part      angle    |
//:                        --------------------------
//:             Rectangular            |               Polar
//:           representation           |           representation
//:     ----------------------------------------------------------------
//:
//: **Figure 2.2.1:** *Structure of the generic complex-arithmetic system.*

//: The resulting complex-number system has the structure shown in Figure 2.21. The system has been decomposed into three relatively independent parts: the complex-number arithmetic operations, Alyssa's polar implementation, and Ben's rectangular implementation. The polar and rectangular implementations could have been written by Ben and Alyssa working separately, and both of these can be used as underlying representations by a third programmer implementing the complex arithmetic procedures in terms of the abstract constructor/selector interface.
//:
//: Since each data object is tagged with its type, the selectors operate on the data in a generic manner. That is, each selector is defined to have a behaviour that depends upon the particular type of data it is applied to. Notice the general mechanism for interfacing the separate representations: Within a given representation implementation (say, Alyssa's polar package) a complex number is an untyped pair (magnitude, angle). When a generic selector operates on a number of polar type, it strips off the tag and passes the contents on to Alyssa's code. Conversely, when Alyssa constructs a number for general use, she tags it with a type so that it can be appropriately recognised by the higher-level procedures. This discipline of stripping off and attaching tags as data objects are passed from level to level can be an important organizational strategy, as we shall see in Section 2.5.


//: ## 2.4.3 Data-Directed Programming and Additivity
//:
//: The general strategy of checking the type of a datum and calling an appropriate procedure is called *dispatching on type*. This is a powerful strategy for obtaining modularity in system design. On the other hand, implementing the dispatch as in Section 2.4.2 has two significant weaknesses. One weakness is that the generic interface procedures (real-part, imag-part, magnitude, and angle) must know about all the different representations. For instance, suppose we wanted to incorporate a new representation for complex numbers into our complex-number system. We would need to identify this new representation with a type, and then add a clause to each of the generic interface procedures to check for the new type and apply the appropriate selector for that representation.
//:
//: Another weakness of the technique is that even though the individual representations can be designed separately, we must guarantee that no two procedures in the entire system have the same name. This is why Ben and Alyssa had to change the names of their original procedures from Section 2.4.1.
//:
//: The issue underlying both of these weaknesses is that the technique for implementing generic interfaces is not *additive*. The person implementing the generic selector procedures must modify those procedures each time a new representation is installed, and the people interfacing the individual representations must modify their code to avoid name conflicts. In each of these cases, the changes that must be made to the code are straightforward, but they must be made nonetheless, and this is a source of inconvenience and error. This is not much of a problem for the complex-number system as it stands, but suppose there were not two but hundreds of different representations for complex numbers. And suppose that there were many generic selectors to be maintained in the abstract-data interface. Suppose, in fact, that no one programmer knew all the interface procedures or all the representations. The problem is real and must be addressed in such programs as large-scale data-base-management systems.
//:
//: What we need is a means for modularizing the system design even further. This is provided by the programming technique known as *data-directed programming*. To understand how data-directed programming works, begin with the observation that whenver we deal with a set of generic operations that are common to a set of different types we are, in effect, dealing with a two-dimensional table that contains the possible operations on one axis and the possible types on the other axis. The entries in the table are the procedures that implement each operation for each type of argument presented. In the complex-number system developed in the previous section, the correspondence between operation name, data type, and actual procedure was spread out among the various conditional clauses in the generic interface procedures. But the same information could have been organized in a table, as shown in Figure 2.22.
//:
//:    Operations |               Types
//:               |     Polar       |      Rectangular
//:               |-----------------|-----------------------
//:    real-part  | real-part-polar | real-part-rectangular
//:    imag-part  | imag-part-polar | imag-part-rectangular
//:    magnitude  | magnitude-polar | magnitude-rectangular
//:    angle      | angle-polar     | angle-rectangular
//:
//: **Figure 2.22:** Table of operations for the complex- number system
//:
//: Data-directed programming is the technique of designing programs to work with such a table directly. Previously, we implemented the mechanism that interfaces the complex-arithmetic code with the two representation packages as a set of procedures that each perform an explicit dispatch on type. Here we will implement the interface as a single procedure that looks up the combination of the operation name and argument type in the table to find the correct procedure to apply, and then applies it to the contents of the argument. If we do this, then to add a new representation package to the system we need not change any existing procedures; we need only add new entries to the table.
//:
//: To implement this plan, assume that we have two procedures, put and get, for manipulating the operation-and-type table:
//:
//: - put(op: String, type: String, item: (ComplexNumber) -> Double) { installs the <item> in the table, indexed by the <op> and the <type>.
//: - get(op: String, type: String) -> (ComplexNumber) -> Double { looks up the <op>, <type> entry in the table and returns the item found there. If no item is found, get returns false.
//:
//: For now, we can assume that put and get are included in our language. In chapter 3 (Section 3.3.3) we will see how to implement these and other operations for manipulating tables.
//:
//: Here is how data-directed programming can be used in the complex-number system. Ben, who developed the rectangular representation, implements his code just as he did originally. He defines a collection of procedures, or a *package*, and interfaces these to the rest of the system by adding entries to the table that tell the system how to operate on rectangular numbers. This is accomplished by calling the following procedure:

typealias ComplexNumberSelector = (ComplexNumber) -> Double
typealias ComplexNumberConstructor = (Double, Double) -> TaggedComplexNumber

var globalSelectorTable = [ComplexNumberType: [String: ComplexNumberSelector]]()
var globalConstructorTable = [ComplexNumberType: [String: ComplexNumberConstructor]]()

func put(op: String, type: ComplexNumberType, item: ComplexNumberSelector) {
    if let typeColumn = globalSelectorTable[type] {
        globalSelectorTable[type]![op] = item
    } else {
        globalSelectorTable[type] = [op: item]
    }
}
func put(op: String, type: ComplexNumberType, item: ComplexNumberConstructor) {
    if let typeColumn = globalConstructorTable[type] {
        globalConstructorTable[type]![op] = item
    } else {
        globalConstructorTable[type] = [op: item]
    }
}

func get(op: String, type: ComplexNumberType) -> ComplexNumberSelector? {
    return globalSelectorTable[type]?[op]
}
func get(op: String, type: ComplexNumberType) -> ComplexNumberConstructor? {
    return globalConstructorTable[type]?[op]
}


func installRectangularPackage() {
    // Internal Procedures
    func realPart(z: ComplexNumber) -> Double { return z.0 }
    func imagPart(z: ComplexNumber) -> Double { return z.1 }
    func magnitude(z: ComplexNumber) -> Double {
        return pow(square(realPartRectangular(z)) + square(imagPartRectangular(z)), 0.5)
    }
    func angle(z: ComplexNumber) -> Double {
        return atan2(imagPartRectangular(z), realPartRectangular(z))
    }
    
    func tag(x: ComplexNumber) -> TaggedComplexNumber {
        return attachTag(.Rectangular, contents: x)
    }
    func makeFromRealImag(x: Double, y: Double) -> TaggedComplexNumber {
        return tag((x, y))
    }
    func makeFromMagAng(r: Double, A: Double) -> TaggedComplexNumber {
        return tag((r * cos(A), r * sin(A)))
    }
    
    // Interface to the rest of the system
    
    put("realPart", type: .Rectangular, item: realPart)
    put("imagPart", type: .Rectangular, item: imagPart)
    put("magnitude", type: .Rectangular, item: magnitude)
    put("angle", type: .Rectangular, item: angle)
    put("makeFromRealImag", type: .Rectangular, item: makeFromRealImag)
    put("makeFromMagAng", type: .Rectangular, item: makeFromMagAng)
}

//: Notice that the internal procedures here are the same procedures from Section 2.4.1 that Ben wrote when he was working in isolation. No changes are necessary in order to interface them to the rest of the system. Moreover, since these procedure definitions are internal to the installation procedure, Ben needn't worry about name conflicts with other procedures outside the rectangular package. To interface these to the rest of the system, Ben installs his real-part procedure under the operation name real-part and the type (rectangular), and similarly for the other selectors. The interface also defines the constructors to be used by the external system. These are identical to Ben's internally defined constructors, except that they attach the tag.
//:
//: Alyssa's polar package is analogous:

func installPolarPackage() {
    // Internal Procedures
    func magnitude(z: ComplexNumber) -> Double { return z.0 }
    func angle(z: ComplexNumber) -> Double { return z.1 }
    func realPart(z: ComplexNumber) -> Double {
        return magnitude(z) * cos(angle(z))
    }
    func imagPart(z: ComplexNumber) -> Double {
        return magnitude(z) * sin(angle(z))
    }
    
    func tag(x: ComplexNumber) -> TaggedComplexNumber {
        return attachTag(.Polar, contents: x)
    }
    func makeFromMagAng(r: Double, A: Double) -> TaggedComplexNumber {
        return tag((r, A))
    }
    func makeFromRealImag(x: Double, y: Double) -> TaggedComplexNumber {
        return tag((pow(square(x) + square(y), 0.5), atan2(y, x)))
    }
    
    // interface to the rest of the system
    put("magnitude", type: .Polar, item: magnitude)
    put("angle", type: .Polar, item: angle)
    put("realPart", type: .Polar, item: realPart)
    put("imagPart", type: .Polar, item: imagPart)
    put("makeFromMagAng", type: .Polar, item: makeFromMagAng)
    put("makeFromRealImag", type: .Polar, item: makeFromMagAng)
}

//: Even though Ben and Alyssa both still use their original procedures defined with the same names as each other's (e.g., realPart), these definitions are now internal to different procedures (see Section 1.1.8), so there is no name conflict.
//:
//: The complex-arithmetic selectors access the table by means of a general "operation" procedure called applyGeneric, which applies a generic operation to some arguments. applyGeneric looks in the table under the name of the operation and the types of the arguments and applies the resulting procedure if one is present:

func applyGeneric(op: String, arg: TaggedComplexNumber) -> Double {
    let type = typeTag(arg)
    let proc: ComplexNumberSelector? = get(op, type: type)
    if let p = proc {
        return p(contents(arg))
    } else {
        fatalError("There is no selector named \(op) for data of type \(type) registered with \(globalConstructorTable)")
    }
}
// I don't see why the book uses a variable number of arguments here.
// Ok, now I do, it is so that we can have functions like add-complex that require multiple arguments. Although that dosen't make sense either. It really only allows handing over a list of data ojects and performing the same procedure on each of them (no matter which type they are). Some what useful I guess. But I think would prefer to have a function that takes a single argument and the apply that to an array of items using map myself.
// Swift's type system feels a bit like a straightjacket here. I do find it easier to work with than Rackets open ended, anything goes approach though.

//: Using applyGeneric, we can define our generic selectors as follows:

// We need to install the packages first otherwise Swift gets unhappy that the applyGeneric function doesn't work.
installRectangularPackage()
installPolarPackage()

func realPart(z: TaggedComplexNumber) -> Double { return applyGeneric("realPart", arg: z) }
func imagPart(z: TaggedComplexNumber) -> Double { return applyGeneric("imagPart", arg: z) }
func magnitude(z: TaggedComplexNumber) -> Double { return applyGeneric("magnitude", arg: z) }
func angle(z: TaggedComplexNumber) -> Double { return applyGeneric("angle", arg: z) }

//: Observe that these do not change at all if a new representation is added to the system.
//:
//: We can also extract from the table the constructors to be used by the programs external to the packages in making complex numbers from real and imaginary parts and from magnitudes and angles. As in Section 2.4.2, we construct rectangular numbers whenever we have magnitudes and angles:

func makeFromRealImag(x: Double, y: Double) -> TaggedComplexNumber {
    let op = "makeFromRealImag"
    let type: ComplexNumberType = .Rectangular
    let proc: ComplexNumberConstructor? = get(op, type: type)
    if let p = proc {
        return p(x, y)
    } else {
        fatalError("There is no selector named \(op) for data of type \(type) registered with \(globalConstructorTable)")
    }
}

// I found this section of the book quite confusing first time through. The applyGeneric function appeared to imply to me that it could be used for all functions, when it is really only used for the selectors. The constructors are handled as a seperate special cases (which could have been done for the selectors as well).

func makeFromMagAng(r: Double, A: Double) -> TaggedComplexNumber {
    let op = "makeFromMagAng"
    let type: ComplexNumberType = .Polar
    let proc: ComplexNumberConstructor? = get(op, type: type)
    if let p = proc {
        return p(r, A)
    } else {
        fatalError("There is no selector named \(op) for data of type \(type) registered with \(globalConstructorTable)")
    }
}

let genericRectangular = makeFrom