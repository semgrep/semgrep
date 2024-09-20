// https://github.com/returntocorp/semgrep/issues/4091
// We want to test that the try-block is properly connected to the
// catch-block despite the try-block execution ends in a `return`.
public class Main {
    public void test1() {
        String str = "safe";
        try {
            return could_throw();
        }
        catch (Throwable t) {
            str = source();
        }
        //ruleid: test
        sink(str);
    }

    public void test2() {
        int cannot_throw = 42;
        String str = "safe";
        try {
            return cannot_throw;
        }
        // vvv dead code
        catch (Throwable t) {
            str = source();
        }
        //OK:
        sink(str);
    }
}
