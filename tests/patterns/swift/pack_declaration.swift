// ERROR: match
func consumeAll<each T>(_ values: repeat each T) where repeat each T: Equatable {
    consume(repeat each values)
}
func consumeAll<T>(_ values: T) where T: Equatable { consume(values) }
