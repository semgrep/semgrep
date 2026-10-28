package struct Item {
    consuming func consume() {}
    borrowing func borrow() {}
}
distributed actor Worker {
    distributed func run(_ value: consuming Item) {}
}
func take(_ value: consuming Item) { use(value) }
