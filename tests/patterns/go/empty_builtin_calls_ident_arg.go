package calls

func f(new func(...int), make func(), value int) {
	// ERROR: match
	new()
	new(value)
	make()
}
