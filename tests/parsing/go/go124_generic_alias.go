package go124

type Box[T any] struct { Value T }
type JobArgs interface { Kind() string }

type Job[T JobArgs] = Box[T]
type MapAlias[K comparable, V any] = map[K]V

type (
    GroupAlias[T any] = Box[T]
    PlainAlias = int
)

func scanThis(xs []int) {
    for range xs {
        println("still scanned")
    }
}
