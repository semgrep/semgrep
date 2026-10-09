package instantiation

func Identity[T any](value T) T { return value }
func Pair[A, B any](a A, b B) {}
var sliceIdentity = Identity[[]int]
var pair = Pair[int, string]
var qualified = other.Pair[int, string]
