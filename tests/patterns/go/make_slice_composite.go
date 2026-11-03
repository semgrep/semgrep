func f() {
	//ERROR:
	a := make([]string, 1)
	//OK:
	b := []string{"test"}
	//OK:
	c := []string{}
	//OK:
	d := []string{"a", "b"}
}
