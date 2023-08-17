import java.lang.Boolean;

class Test {
    public void ok1(String x) {
        //OK: test
        sink("something" + (x != "safe"));
    }
    public void ok2(String x) {
        //OK: test
        sink(Boolean.valueOf(x));
    }
    public void ok3(String x) {
        //OK: test
        sink(Boolean.parseBoolean(x));
    }
    public void bad(String x) {
        //ruleid: test
        sink("something" + x);
    }
}