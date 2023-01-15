package Foo;

class A {
    void test() {
	//ERROR: match
	int b = 2, a = 1;
	foo();
	while(true) {
	    foo();
	};
	bar();
    }
}

