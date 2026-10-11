package go124

import "time"

type Box[T any] struct{ Value T }
type Pair[A, B any] struct{ A A; B B }

type SliceMap interface { ~[]byte | ~map[string]bool }
type GenericInstances interface { Box[int] | Box[string] }
type StructThenSlice interface { ~struct{ A int } | ~[]byte }
type SliceThenStruct interface { ~[]byte | ~struct{ A int } }
type QualifiedMix interface { int | time.Duration | ~[]byte }
type FuncChannel interface { ~func(int) bool | ~chan int }
type MultiArgGeneric interface { Pair[int, string] | Pair[string, int] }
type WithMethod interface {
    ~[]byte | ~map[string]bool
    Transform() int
}
type EmbeddedWithMethod interface { SliceMap; Get() int }
type CompositeConstraint[T interface{ ~[]byte | ~map[string]bool }] struct{ Value T }
type CompositeAlias[T interface{ Box[int] | Box[string] }] = []T

func stillScanned(xs []int) {
    for range xs { println("observed") }
}
