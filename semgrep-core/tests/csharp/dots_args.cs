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

        // ERROR:
        Foo(2, 1);

        // ERROR:
        Foo(1, 2, 3);

        Bar(1, 2);
    }

    private static void Foo(int a, int b, int c = 3)
    {
    }

    private static void Bar(int a, int b)
    {
    }
}