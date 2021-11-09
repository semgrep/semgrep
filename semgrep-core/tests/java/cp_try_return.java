// https://github.com/returntocorp/semgrep/issues/4091
public class Main {
    public boolean test1() {
        Throwable throwable = null;
        while (cond) {
            try {
                return could_throw();
            }
            catch (Throwable t) {
                throwable = t;
            }
        }
        //OK:
        return throwable == null;
    }

    public boolean test2() {
        int cannot_throw = 42;
        Throwable throwable = null;
        while (cond) {
            try {
                return cannot_throw;
            }
            catch (Throwable t) {
                // dead code
                throwable = t;
            }
        }
        //ERROR:
        return throwable == null;
    }

   public boolean test3() {
        Throwable throwable = null;
        try {
            return could_throw();
        }
        catch (Throwable t) {
            throwable = t;
        }
        //OK:
        return throwable == null;
    }
}
