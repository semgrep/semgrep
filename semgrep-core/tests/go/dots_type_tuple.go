package main

func test() (error, int, string) {
	return nil, 0, "hello"
}

//ERROR: match
func test2() (int, error) {
	return 0, nil
}

//ERROR: match
func test3() (int, int, error) {
	return 0, 1, nil
}

func test4() (int, error, string) {
	return 0, nil, "hello"
}

/**
 * error is parsed as a single type and thus would not count as a tuple
 */
func test5() error {
	return nil
}
