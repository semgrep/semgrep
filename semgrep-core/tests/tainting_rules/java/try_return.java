// https://github.com/returntocorp/semgrep/issues/4091
public class Main {
    public void test1() {
        String str = "safe";
        try {
            return could_throw();
        }
        catch (Throwable t) {
            str = source();
        }
        //ERROR:
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
