class Test {
    Properties fld;
    
    void main() {
	Properties x1;

	// The classic matches
	
	//ERROR: match
	foo(x1);
	//ERROR: match
    	foo(this.fld);
	//ERROR: match
	foo(new Properties());

	// and the new ternary one!

	//ERROR: match
	foo(this.properties == null ? new Properties() : this.fld);
}
}
