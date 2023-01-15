class A {
    void foo() {
	// == used to have the wrong precedence in tree-sitter-java
	// which prevented semgrep to match such code with $X == $X
	//ERROR: match
	return a+b == a+b;
    }
}
