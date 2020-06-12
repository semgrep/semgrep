public class Bar {
   int x;
   int y;
}

public class Foo {
    void f() {
	String str = "hello";
        Bar bar;
	Point p;
	
	//Todo should be matched
	bar.x = make(str, p);
	//Todo should be matched
	bar.y = make(str);
	p.x = make(str);
	p.y = make(str);
   }
}
