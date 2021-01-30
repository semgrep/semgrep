using System;
using System.IO;

class HelloWorldUsing
{
    public static void Main()
    {
        UsingBlock();
        UsingDecl();
    }

    private static void UsingBlock()
    {
        using (var reader = new StringReader("hello world"))
        {
            Console.WriteLine(reader.ReadToEnd());
        }
    }

    private static void UsingDecl()
    {
        using var reader = new StringReader("hello world");
        Console.WriteLine(reader.ReadToEnd());
    }
}