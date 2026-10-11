package go124

import "time"

type NamedThenStruct interface { string | ~struct{ A int } }
type StructThenNamed interface { ~struct{ A int } | time.Duration }
type NamedStructNamed interface { string | ~struct{ A int } | time.Duration }
type MixedThreeStructs interface {
    ~struct{ A int } | ~struct{ B string } | time.Duration
}
type MixedWithComposite interface { string | ~struct{ A int } | ~map[string]bool }
type HomogeneousStruct interface { ~struct{ A int } | ~struct{ B string } }
type NamedStructConstraint[T interface { string | ~struct{ A int } }] = []T

func stillScannedNamed(xs []int) { for range xs { println("seen") } }
