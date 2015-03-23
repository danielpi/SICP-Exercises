import Cocoa

// Exercise 2.4
// Here is an alternative procedural representation of pairs. For this representation, verify that car(cons(x, y)) yields x for any objects x and y.

func cons2(x: Int, y: Int) -> ((Int, Int) -> Int) {
    return { m in
        m(x, y)
    }
}
//((Int, Int) -> Int)
func car2(z:((Int, Int) -> Int) ) -> Int {
    return z({ p, q in
        return p
    })
}

1 + 2
