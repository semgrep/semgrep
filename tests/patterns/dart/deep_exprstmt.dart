int main() {
	//ERROR: match
        foo();
	bar();
	//ERROR: match
        foo();
	int x = bar();
	//ERROR: match
        foo();
	foo(bar());
	//ERROR: match
        foo();
	return bar();
}
