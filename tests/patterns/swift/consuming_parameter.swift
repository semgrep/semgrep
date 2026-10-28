struct Item {}
// ERROR: match
func take(_ value: consuming Item) { use(value) }
consuming func consume() {}
func borrow(_ value: borrowing Item) { use(value) }
