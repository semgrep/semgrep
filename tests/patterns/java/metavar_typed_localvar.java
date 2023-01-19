public class Bar {
   int x;
   int y;
}

public class Foo {
    void f(Bar[] bars) {
        Bar[] bars2;
	Bar bar;
	
	//ERROR: match
        if (is_good(bars[0])) return;
	if (is_good(bar)) return;
	//ERROR: match
        if (is_good(bars2[1])) return;
   }
}
