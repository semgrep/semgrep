// `repeat(x)` parses as a pack expansion, not a call: `repeat` is not boosted
// as an identifier because that breaks `repeat { ... } while` loops.
// ERROR: match
consume(repeat(value))
// ERROR: match
consume(repeat value)
consume(each(value))
