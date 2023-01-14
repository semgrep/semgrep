class Test {
    String x;

    public void foo() {
        x = tainted;
        //ruleid: test
        sink(x);
    }

    public void bar() {
        this.x = tainted;
        //ruleid: test
        sink(this.x);
    }

    public void baz() {
        this.x = tainted;
        //ruleid: test
        sink(x);
    }
}
