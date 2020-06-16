public class Foo {
    private void bar() {
        int x = 1;
        float y = 2.2;
        String str = "hello";
        //ERROR:
        f(1, 2.2, "hello");
        //ERROR:
        f(x, y, str);
        f(y, s, str);
        f(true);
    }
}
