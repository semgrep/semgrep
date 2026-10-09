package embedding

// ERROR: match
type Plain interface {
	Reader
}

// ERROR: match
type Generic interface {
	Reader[int]
}

// ERROR: match
type Qualified interface {
	io.Reader[int]
}

type Constrained interface {
	~int
}
