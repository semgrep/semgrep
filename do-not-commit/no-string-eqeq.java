public class Example {
    public int foo(String a, int b) {
        // ruleid:no-string-eqeq
        if (a == "hello") return 1;
        // ok
        if (b == 2) return -1;
        // ruleid:no-string-eqeq
        if ("hello" == a) return 2;
        //ok
        if (null == "hello") return 12;
        //ok
        if ("hello" == null) return 0;
    }
}
