class Test 
{
    public static void Main()
    {
        // OK:
        Foo(42, "baz");

        // ERROR:
        Foo(bar: 42, baz: "baz");
        // ERROR:
        Foo(baz: "baz", bar: 42);
    }
}