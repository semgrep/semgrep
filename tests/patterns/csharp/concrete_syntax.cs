public class ConcreteSyntax
{
    public static void Main()
    {
        // ERROR:
        Foo(1, 2);

        // ERROR:
        Foo(1,
            2);

        // ERROR:
        Foo(1, // comment
            2);

        Foo(2, 1);

        Foo(1, 2, 3);
    }

    private static void Foo(int a, int b, int c = 3)
    {
    }
}