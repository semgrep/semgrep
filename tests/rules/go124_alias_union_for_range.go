package go124

import "time"

type Box[T any] struct { Value T }
type BoxAlias[T any] = Box[T]
type DurationSet interface { int | time.Duration }

func rangeWithModernTypes[T ~int | ~int64](xs []int, value T) {
    // ruleid:go124-alias-union-range
    for range xs {
        println(value)
    }
    // ok:go124-alias-union-range
    for i := 0; i < len(xs); i++ {
        println(xs[i])
    }
}
