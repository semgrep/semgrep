using System;

class HelloWorldIndexer
{
    public static void Main()
    {
        checked { Overflow(() => int.MaxValue + int.Parse("10")); }
        unchecked { Overflow(() => int.MaxValue + int.Parse("10")); }
        Overflow(() => checked(int.MaxValue + int.Parse("10")));
        Overflow(() => unchecked(int.MaxValue + int.Parse("10")));
    }

    public static void Overflow(Func<int> action)
    {
        try
        {
            action();
            int i = int.MaxValue + int.Parse("10");
            Console.WriteLine("hello");
        }
        catch
        {
            Console.WriteLine("world");
        }
    }
}