class Foo {
    //ERROR: match
    static void foo(double... bar) {
	return bar;
    }
}
