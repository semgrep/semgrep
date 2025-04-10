import java.lang.Integer;
import java.lang.Short;
import java.lang.Long;

class Test {
    public void test1(int x) {
        //OK: test
        sink(x);
    }
    public void test2(long x) {
        //OK: test
        sink(x);
    }
    public void test3(Object x) {
        long t = x.getSomething();
        //OK: test
        sink(t);
    }
    public void test4(Object[] x) {
        //OK: test
        sink(x.length);
    }
    public void test5(Object x) {
        var t = (int)x.getSomething();
        //OK: test
        sink(t);
    }
    public void test6(Object[] x) {
        var u = 1;
        var v = u + 1;
        var w = v + x.length;
        //OK: test
        sink(w);
    }
    public void test7(Object x) {
        var u = "a";
        var v = "a" + 1;
        var w = v + x.getSomething();
        //ruleid: test
        sink(w);
    }
    public void test8(String x) {
        //OK: test
        sink(Integer.valueOf(x));
    }
    public void test9(String x) {
        //OK: test
        sink(Integer.parseInt(x));
    }
    public void test10(String x) {
        //OK: test
        sink(x.compareTo("safe"));
    }
    public void test11(short x) {
        //OK: test
        sink(x);
    }
    public void test12(Short x) {
        //OK: test
        sink(x);
    }
    public void test13(Integer x) {
        //OK: test
        sink(x);
    }
    public void test14(Long x) {
        //OK: test
        sink(x);
    }
}
