package aliases

// ERROR: match
type Values[T any] = map[string]T
// ERROR: match
type Grouped[T any] /* alias */ = map[string][]T
type IntegerKeys[T any] = map[int]T
type Defined[T any] map[string]T
// ERROR: match
type Ordinary = map[string]int
