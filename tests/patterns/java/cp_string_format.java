public class Test {

    public static void test1() {
        String key = "1234567";
        String str = String.format("Abc %s",key);
        //MATCH:
        foobar(str);
    }

    public static void test2() {
        String key = "1234567";
        String str = java.lang.String.format("Abc %s",key);
        //MATCH:
        foobar(str);
    }

    public static void test3() {
        String key = "1234567";
        //MATCH:
        foobar(String.format("Abc %s",key));
    }

    public static void test4() {
        String key = "1234567";
        //MATCH:
        foobar(java.lang.String.format("Abc %s",key));
    }

}
