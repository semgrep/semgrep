public class MetaVar
{
    public static void Main()
    {
        // ERROR:
        Foo(1, 2);

        // ERROR:
        Foo(int.MaxValue,
            2);

        // ERROR:
        Foo(int.Parse("3"), // comment
            2);

        // ERROR:
        Foo(Bar(1, 3), 2);

	// OK:
        Foo(2, 1);

	// OK:
	Foo(1, 2, 3);
    }
}
