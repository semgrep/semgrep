func f() {

	switch {
		case x < y:
			a := 0;
		case x > y:
			b := 0;
		//ERROR:
		case sink(source):
			c := 0;
		default:
			c := 0;
	}

}