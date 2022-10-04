class Foo {
    // ERROR:
    void foo() {
        foo(1,2);
    }

    // ERROR:
    public static void bar(int bar1, int bar2, int bar3) {
        foo(1,2,3);
        bar(1,2);
    } 

    // ERROR:
    public void foobar(int bar1) {
        foo();
    }

    public static String str() {
        return "hello";
    }
}
