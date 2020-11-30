// ERROR:
public class Foo {
    int foo = 5;

    public static void foo() {
        int foo1 = 4;
    }
}

// ERROR:
private class Bar extends Foo {
    int bar = 3;
}

// ERROR:
abstract class Foobar {
    public abstract void foobar();
}

