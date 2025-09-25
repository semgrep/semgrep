public class A{
    public static int test1() {
        int a = 1;
        // ruleid: multi-or
        return a;
    }
    public static int test2() {
        int a = 2;
        // ok: multi-or
        return a;
    }
}
