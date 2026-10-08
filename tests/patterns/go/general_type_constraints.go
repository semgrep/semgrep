package constraints

// ERROR: match
type Marked interface {
	~[]int | ~map[string]int
	Marker()
}

type Unmarked interface {
	~[]int | ~map[string]int
	Other()
}
