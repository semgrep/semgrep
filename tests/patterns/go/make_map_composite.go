func f() {
	//ERROR:
	a := make(map[string]int, 1)
	//OK:
	b := map[string]int{"test": 1}
	//OK:
	c := map[string]int{}
}
