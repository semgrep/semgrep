import java.lang.Integer;

class Test {
    public void test(Object x) {
        var u = "a";
        var v = "a" + 1;
        var w = v + x.getSomething();
        //ruleid: test
        sink(w);
    }
    public void testok(String x) {
        //OK: test
        sink(x.compareTo("safe"));
    }
}
