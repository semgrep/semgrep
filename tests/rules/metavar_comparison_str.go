import (
    "math"
)

func divide_remainder(a int, b int) (int,int) {
     return math.Floor(a/b), a%b
}


// pointless function, but example of the bad pattern
func bad(a int, b int, c int) int {
    foo, remainder :=  divide_remainder(a, b)
    if (c != 0) {
        //ruleid: reinstantiated_variable_in_new_block
        _, remainder := divide_remainder(foo, c) //bad line
    }
    return remainder
}

// pointless function, but example of a non problem
func bad(a int, b int, c int) int {
    foo, remainder :=  divide_remainder(a, b)
    if (c != 0) {
        //ok: reinstantiated_variable_in_new_block
        _, remainder2 := divide_remainder(foo, c) //ok line
    }
    return remainder
}

// pointless function, but example of the acceptable pattern
func good(a int, b int, c int) int {
    foo, remainder :=  divide_remainder(a, b)
    if (c != 0) {
        //ok: reinstantiated_variable_in_new_block
        _, remainder = divide_remainder(foo, c) //good line
    }
    return remainder
}
