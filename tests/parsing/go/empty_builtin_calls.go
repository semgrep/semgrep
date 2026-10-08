package calls

// The names new and make may be shadowed by ordinary functions.
func calls(new func(), make func()) {
	new()
	make()
}
