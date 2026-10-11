package go124

func useSliceOrMap[T ~[]byte | ~map[string]bool](value T) T {
    return value
}

func useStructs[T ~struct{ A int } | ~struct{ B int }](value T) T {
    return value
}

func useArrayOrPointer[T ~[3]int | ~*int](value T) T {
    return value
}

func useChannelsOrFunctions[T ~chan int | ~func() int](value T) T {
    return value
}
