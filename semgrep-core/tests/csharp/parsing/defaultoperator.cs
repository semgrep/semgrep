using System;

class HelloWorldDefault
{
    public static void Main()
    {
        var hello = default(string) + "hello";
        var world = World("world");
        var excl = Exclamation(default);
        Console.WriteLine(hello + world + excl);
    }

    private static string World<T>(T param)
    {
        var t = default(T);
        return t + param.ToString();
    }

    private static string Exclamation(string param)
    {
        return param + "!";
    }
}