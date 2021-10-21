class Foo {
    void foo() {
        //ERROR: match
        JexlEngine jexl = new JexlBuilder().create();
	
        //ERROR: match
	JexlEngine jexl = new JexlBuilder().cache(512).create();
        //ERROR: match
	JexlEngine jexl = new JexlBuilder().cache(512).strict(false).create();
        //ERROR: match
	JexlEngine jexl = new JexlBuilder().cache(512).strict(false).silent(false).create();
	
	JexlEngine jexl = "new JexlBuilder().cache(512).strict(false).silent(false).create()";
    }
}
