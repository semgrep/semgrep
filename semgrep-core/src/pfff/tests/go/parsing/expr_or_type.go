package foo;


func f2(arg int) (int, error) {
	if arg == 42 {

		// In this case we use `&argError` syntax to build
		// a new struct, supplying values for the two
		// fields `arg` and `prob`.
		return -1, &argError{arg, "can't work with it"}
	}

	// This `mutex` will synchronize access to `state`.
	var mutex = &sync.Mutex{}

	return arg + 3, nil

}


func issue13264() {
    // this can be parsed incorrectly by parsing hack as
    // for ; ; []map[int]int { }   and later as [0][0] = 0 {
    // which then can cause an error in expr_to_type because 0 is not
    // a valid type for CompositeLit
    // The solution is to parse this correctly
	for ; ; []map[int]int{}[0][0] = 0 {
	}

}

func foo() {
	c.Assert(len(cfg.Get("menus.docs").(([]map[string]interface{}))), qt.Equals, 2)

}
