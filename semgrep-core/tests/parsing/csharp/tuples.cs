using System;

public class HelloWorldTuples
{
    private static (string, string) MyGreeting;
    private static (string Head, string Tail) MyNamedGreeting;

    public static void Main()
    {
        Console.WriteLine(new Tuple<string, string>("Hello", "World"));
        Console.WriteLine(("Hello", "World"));
        Console.WriteLine(GetGreeting());
        Console.WriteLine(GetNamedGreeting());

        var (a, b) = GetGreeting();
        Console.WriteLine(a + b);

        (string c, string d) = GetGreeting();
        Console.WriteLine(c + d);
        
        (var e, var f) = GetGreeting();
        Console.WriteLine(e + f);

        string g, h;
        (g, _) = GetGreeting();
        (_, h) = GetGreeting();
        Console.WriteLine(g + h);

        Console.WriteLine(("Hello", "World") == GetGreeting());

        MyGreeting = ("Hello", "World");
        Console.WriteLine(MyGreeting);

        (string, string) i, j = GetGreeting();
        Console.WriteLine(j);

        (string, string)[] greetings = { GetGreeting(), GetGreeting() };
        foreach (var (k, l) in greetings) {
            Console.WriteLine(k + l);
        }

        for (var (m, n) = GetGreeting(); m != "World"; (n, m) = (m, n))
        {
            Console.WriteLine(m + n);
        }
    }

    public static (string, string) GetGreeting()
    {
        return ("Hello", "World");
    }

    public static (string Head, string Tail) GetNamedGreeting()
    {
        return ("Hello", "World");
    }
}