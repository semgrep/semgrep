class Test {

    private int x;

    public void test1() {
        //ruleid:test
        foo(this.x);
    }

    public void test2(int x) {
        //ruleid:test
        foo(this.x);
    }

    public void test3() {
        //ok:test
        foo(this.y);
    }

    public void test4(int x) {
        //ok:test
        foo(x);
    }

}
