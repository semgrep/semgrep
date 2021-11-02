public class Test
{
    // ERROR:
    public static int Test()
    {
        Foo(1, 2);
        return 1;
    }

    // ERROR:
    int Test2() {
        return 1;
    }

    // ERROR:
    public static void Main()
    {
        Foo(1, 2);
        return 1;
    }
}
