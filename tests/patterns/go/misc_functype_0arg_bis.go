package foo

//ERROR: match
func asd(a func()) {
	a()
}
