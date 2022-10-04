package Foo

func bar() {
	var a = 1
	var b = 2
	var c = 3
	//ERROR:
	if a > b {
		c = 0
	}
}
