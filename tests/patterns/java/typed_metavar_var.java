class Foo {
    void test1() {
	var sb = new StringBuilder("abc");
	//ERROR: match
	sb.append('c');
    }
}
