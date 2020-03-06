package Foo

func foo() {
	//ERROR:
	var x = "foobar"
	if true {
		x = "bar"
	}
	var z = 3
}
