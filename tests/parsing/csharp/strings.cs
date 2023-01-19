using System;

class HelloWorldString
{
    public static void Main()
    {
        var planet = "world";
        string[] greetings =
        {
            "hello\\worl\x0064",
            @"hello\world",
            $"hello\\{planet}",
            $@"hello\{planet}",
            @$"hello\{planet}",
        };

        foreach (var g in greetings)
        {
            Console.WriteLine(g);
        }
    }
}