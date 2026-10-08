package typesets

type Box[T any] struct{ Value T }

type Number interface {
	~int | ~float64
}

type Underlying interface {
	~Box[int]
}

type Embedded interface {
	Reader
	io.Writer
}

var unionArgument Box[int | string]
var underlyingArgument Box[~int]

func Lookup[K comparable, V int | string](key K, value V) {}
