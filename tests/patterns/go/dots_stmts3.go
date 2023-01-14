package Foo

func foo() {
	//ERROR:
	var x = "foobar"
	//var y = "bla";
	if true {
		x = "bar"
	}
	var z = 3
	if (false) { 
		// ignore me
	}
	var d1 = x + z;
}
