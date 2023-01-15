using System;

class HelloWorldFinalizer
{
    public static void Main()
    {
        new HelloWorldFinalizer();
    }

    ~HelloWorldFinalizer()
    {
        Console.WriteLine("Hello world");
    }
}