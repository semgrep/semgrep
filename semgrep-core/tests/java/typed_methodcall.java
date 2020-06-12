public class Foo {
    private void bar() {
        int x = 1;
        //ERROR:
        f(1);
        //ERROR:
        f(x);
        float y = 2;
        f(y);
        f("string");
    }
}
