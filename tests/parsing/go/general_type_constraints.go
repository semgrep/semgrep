package constraints

type Composite interface {
	~[]int | ~map[string]int
	~func(int) string
	~chan int
	*int | [2]string
}

type Pair[A, B ~[]int | ~[]string] struct {
	First A
	Second B
}

func Identity[T ~[]int | ~[]string](value T) T { return value }

type Box[T any] struct { Value T }
type Reader[T any] interface { Read() T }
type Nested interface {
	~[]Box[int]
	Reader[int]
}
