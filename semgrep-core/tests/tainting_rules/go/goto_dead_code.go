func f() {
	goto LABEL
	//OK:
	sink(source)
	LABEL:
	//ERROR: match
	sink(source)
}