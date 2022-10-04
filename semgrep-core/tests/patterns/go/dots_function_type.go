package main

//ERROR: match
func d(f func(i int) int) {
}

//ERROR: match
func a(f func()) {
}
//ERROR: match
func b(f func(i int)) {
}
//ERROR: match
func c(f func(i int) int) {
}
//ERROR: match
func d(i int, f func(i int) int, j int) {
}

// this does not match, you need to name the argument
func abis(func()) {
}

func main() {
}
func b(i string) {
}
