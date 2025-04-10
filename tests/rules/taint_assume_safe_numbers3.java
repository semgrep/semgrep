class Data {
    int x;
}

class Test {
    int y;


    public void test_bad() {
        String v = source();
        //ruleid: test
        sink(v);
    }


    public void test2_fun() {
        //ok: test
        sink(this.y);
    }

    public test2() {
        this.y = source();
        test2_fun();
    }


    public void test3_fun(Data data) {
        // Requires Pro naming/typing to know that `data.x` has type `int`,
        // OSS naming does not understand class declarations.
        // protodook: deepok: test
        sink(data.x);
    }

    public void test3(Data data) {
        Data data = new Data(source());
        test3_fun(data);
    }
}
