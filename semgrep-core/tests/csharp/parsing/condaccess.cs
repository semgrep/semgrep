using System;

class HelloWorldCondAccess
{
    private static string MaybeNull(int arg)
    {
        if (arg > 0)
        {
            return "Hello";
        }

        return null;
    }
    public static void Main()
    {
        Console.WriteLine(MaybeNull(0)?.Length);
        Console.WriteLine(MaybeNull(1)?.Length);
        var greeting = MaybeNull(0);
        Console.WriteLine(greeting?.Length);
    }
}