package Foo

func bar() {
	baz := 0
	//ERROR: match
	foo(baz + 42)
}
