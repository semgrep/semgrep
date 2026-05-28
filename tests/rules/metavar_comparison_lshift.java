public class A{
    public static int test1() {
        //ruleid: test
        return 2 << 1;
    }
    public static int test2() {
        //ok: test
        return 3 << 1;
    }
    public static int test3() {
        //ruleid: test-negative
        return -2 << 1;
    }
    public static int test4() {
        //ok: test-negative
        return -3 << 1;
    }
}
