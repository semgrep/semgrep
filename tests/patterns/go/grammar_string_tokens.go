package strings

func f() {
	// ERROR: match
	sink(``)
	// ERROR: match
	sink("")
	sink(` `)
	sink(`not empty`)
}
