import Cocoa

func square(x: Int) -> Int { return x * x }

// Exercise 2.23
// The procedure for-each is similar to map. It takes as arguments a procedure and a list of elements. However, rather than forming a list of the results, for-each just applies the procedure to each of the elements in turn, from left to right. The values returned by applying the procedure to the elements are not used at all -- for-each is used with proedures that perform an action, such as printing. For example,

for x in [57, 321, 88] {
    print("\(x)")
}

// The value returned by the call to for-each (not illustrated above) can be something arbitrary, such as true. Give an implmentation of for-each

func forEach<T>(proc: (T) -> (), items: List<T>) {
    if !items.isEmpty() {
        proc(car(items)!)
        forEach(proc, items: cdr(items)!)
    }
}

forEach({ x in print("\(x)") }, items: [57, 32