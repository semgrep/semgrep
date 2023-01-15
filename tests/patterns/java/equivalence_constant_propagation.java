class Foo {
	final String pwd = "password";	
 	void main()	{
        //ERROR: match
		foo(pwd);
	}

    void diff_word() {
        foo("hello");
    }
    void diff_func() {
        bar("password");
    }

    void same_const() {
        //ERROR: match
        foo("password");
    }

    void all_diff() {
        bar("hello");
    }
}
