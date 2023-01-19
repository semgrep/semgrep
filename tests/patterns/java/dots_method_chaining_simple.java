class Foo {
    void foo() {
	//ERROR: match
	o.create();
	//o.foo().create();
    }
}
