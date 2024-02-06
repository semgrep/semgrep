class Test {
    void test() {
        Foo x = new Foo();
        x.t = taint_source();
        Foo y = x;
        //ERROR:
        sink(y.t, z);
    }
}
