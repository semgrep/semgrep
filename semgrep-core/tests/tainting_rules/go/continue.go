func f() {
	i := 0
	for i < 10 {
		i := i + 1
		continue
		//OK:
		sink(source)
	}
	//ERROR:
	sink(source)
}