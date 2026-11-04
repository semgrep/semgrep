package aliases

type Values[T any] = map[string]T
type Pairs[K comparable, V any] = map[K]V
type Signed[T ~int | ~int64] = []T
type (Grouped[T any] /* alias */ = []T)
