class Foo {
    // ERROR:
    @Override
    public void foo1() {
        int x = 1;
    }

    // ERROR:
    @Anno1
    @Anno2
    public void foo2() {
        int x = 2;
    }

    public void foo3() {
        int x = 3;
    }
}
