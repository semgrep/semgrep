package embedded

// ERROR: match
type WithString struct {
	Box[int]
	*other.Box[string]
	Value string
}

type WithInt struct {
	Box[int]
	Value int
}
