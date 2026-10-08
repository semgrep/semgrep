package calls

func f(new func(...int), make func()) {
	// ERROR: match
	new()
	new(1)
	make()
}
