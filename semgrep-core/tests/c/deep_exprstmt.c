int main(int argc, char *argv[]) {
	//ERROR: match
	bar();
	//ERROR: match
	int x = bar();
	//ERROR: match
	foo(bar());
	//ERROR: match
	return bar();
}
