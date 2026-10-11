package go124

import "time"

type SafeBytes interface { ~[]byte | ~map[string]bool }
type Box[T any] struct{ Value T }
type GenericBound interface { Box[int] | Box[string] }

func sink(value any) {}

func scanInterface[T SafeBytes](xs []int, value T) {
    // ruleid:go124-interface-range
    for range xs {
        // ruleid:go124-interface-sink
        sink(value)
    }
    // ok:go124-interface-range
    for i := 0; i < len(xs); i++ { println(xs[i]) }
}

type NamedWithStruct interface { string | ~struct{ A int } | time.Duration }

func scanNamedWithStruct[T NamedWithStruct](xs []int, value T) {
    // ruleid:go124-interface-range
    for range xs {
        // ruleid:go124-interface-sink
        sink(value)
    }
}

type ParenthesizedInterfaceConstraint interface {
    interface{ ~int } | ~([]byte)
}

func scanParenthesizedInterfaceConstraint[T ParenthesizedInterfaceConstraint](xs []int, value T) {
    // ruleid:go124-interface-range
    for range xs {
        // ruleid:go124-interface-sink
        sink(value)
    }
    // ok:go124-interface-range
    for i := 0; i < len(xs); i++ { println(xs[i]) }
}
