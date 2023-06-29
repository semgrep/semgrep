public class Example {
    public int foo(String a, int b) {
        // ok: no-string-eqeq
        if ("hello" == a) return 2;
    }
}
