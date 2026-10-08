package identifiers

func f() {
	// ERROR: match
	consume(value١)
	// ERROR: match
	consume(value२)
	ignore(value١)
}
