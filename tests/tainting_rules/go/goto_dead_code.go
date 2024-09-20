func f() {
	goto LABEL
	//OK:
	sink(source)
	LABEL:
	//ruleid: test-dead-code
	sink(source)
}
