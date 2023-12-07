public class Test {

    public static void test1() {
        String str = Config.load('str');
        //ok: test
        sink1("Abc", String.format("Bearer %s", str));
    }

    public static void test2() {
        String str = "45678";
        //ruleid: test
        sink2("123", str);
    }

}

