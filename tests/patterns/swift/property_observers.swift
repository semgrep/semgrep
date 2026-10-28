class Counter {
// ERROR: match
var count = 0 { willSet(next) { log(next) } didSet(old) { log(old) } }
var other = 0 { didSet { log(oldValue) } }
}
