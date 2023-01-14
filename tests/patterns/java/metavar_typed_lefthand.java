public class Bar {
   int x;
   int y;
}

public class Foo {
    void f(Bar[] bars) {
        Bar[] bars2;
	Bar bar;
	
	//ERROR: match
	bars2 = NULL;
	bar = NULL;
   }
}
