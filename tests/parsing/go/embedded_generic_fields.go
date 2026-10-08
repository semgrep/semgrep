package embedded

type Box[T any] struct { Value T }
type Record struct {
	Box[int]
	*Box[string]
	other.Box[[]byte]
}
