class Counter {
    var count = 0 {
        willSet(next) { log(next) }
        didSet(old) { log(old) }
    }
    var other: Int {
        didSet { log(oldValue) }
        willSet { log(newValue) }
    }
}
