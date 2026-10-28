class Counter {
// ERROR: match
var other = 0 { didSet(old) { log(old) } willSet(next) { log(next) } }
var count = 0 { willSet(next) { log(next) } didSet(old) { log(old) } }
}
