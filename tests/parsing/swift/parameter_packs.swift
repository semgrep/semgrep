func consumeAll<each T>(_ values: repeat each T) where repeat each T: Equatable {
    consume(repeat each values)
}
func pair<each T, each U>(_ left: repeat each T, _ right: repeat each U)
    where repeat each T == repeat each U {
    consume(repeat (each left, each right))
}
