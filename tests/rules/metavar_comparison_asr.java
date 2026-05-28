public class A{
    public static int test1() {
        //ruleid: test
        return 8 >> 1;
    }
    public static int test2() {
        //ok: test
        return 8 >> 2;
    }
    public static int test3() {
        //ruleid: test-negative
        return -8 >> 1;
    }
    public static int test4() {
        //ok: test-negative
        return -8 >> 2;
    }
}
