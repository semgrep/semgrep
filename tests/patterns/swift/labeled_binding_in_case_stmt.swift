public var foo: String {
   // MATCH:
    switch self {
        case .bar(_, _, x: let y):
            return y
    }
    // OK:
    switch self {
        case .baz(let a):
            return a
    }
}

