using System;
using System.IO;
using static System.Math;
using Foo = System.Text;

class HelloWorldUsing
{
    public static void Main()
    {
        UsingBlock();
        UsingDecl();
        UsingStatic();
        UsingAlias();
    }

    private static void UsingAlias()
    {
        var builder = new Foo.StringBuilder("hello world");
        Console.WriteLine(builder.ToString());
    }

    private static void UsingStatic()
    {
        Console.WriteLine(PI);
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