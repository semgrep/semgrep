
class Foo {
    void main() {
	//ERROR: match
	for (List<Integer> int: integers) {
	    someFunc();
	}
    }
}
