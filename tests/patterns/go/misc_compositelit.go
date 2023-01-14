package Foo

func transport() {
	//ERROR: match	
	c := http.DefaultClient
	// the {} used to not be in the generic AST, leading to bad autofix
	c.Transport = &http.Transport{}
}
