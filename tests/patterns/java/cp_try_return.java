// https://github.com/returntocorp/semgrep/issues/4091
// We want to test that the try-block is properly connected to the
// catch-block despite the try-block execution ends in a `return`.
public class Main {
    public boolean test1() {
        Throwable throwable = null;
        // Here the `while` fall-through case ensures that the `return` is
        // reachable, so if the catch-block is not processed we will get
        // a false positive.
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
            // try-block returns without raising ay exception so the
            // catch-block is unreachable!
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
        // Not a meaningful test here since this worked with and without the bug
        // (the bug caused the `return` to be unreachable), but it was part of
        // the bug report so I kept it.
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
