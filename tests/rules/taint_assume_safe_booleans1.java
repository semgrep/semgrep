class Test {
    public void ok(String x) {
        //OK: test
        sink("something" + (x != "safe"));
    }
    public void bad(String x) {
        //ruleid: test
        sink("something" + x);
    }
}