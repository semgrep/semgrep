public class Example {
    public int foo(String a, int b) {
	//ERROR:
        if (a == "hello") return 1;
        if (b == 2) return -1;
    }
}

