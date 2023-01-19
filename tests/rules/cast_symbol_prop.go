
func foo() {
	// Note that the Go parser parses this to a Cast of a ParenExpr
	x := f([]int("foo"))

	// ruleid: cast-symbol-prop
	sink(x)
}