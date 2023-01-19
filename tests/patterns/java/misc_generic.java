class Foo {
    void foo() {
	//ERROR: match
	A<int> x = (A<int>)qux.foo("hello");
    }
}

