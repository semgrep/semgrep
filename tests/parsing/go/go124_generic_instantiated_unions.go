package go124

type Box[T any] struct { Value T }
type BoxAlias[T any] = Box[T]

func useInstantiatedTypes[T Box[int] | Box[string]](value T) T {
    return value
}

func useAliasedInstantiatedTypes[T BoxAlias[int] | BoxAlias[string]](value T) T {
    return value
}
