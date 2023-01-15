using System;

class HelloWorldTypeConstraints : IRunnable
{
    public static void Main()
    {
        var hello = CreateT<HelloWorldTypeConstraints>();
        hello.Run();
    }

    private static T CreateT<T>() where T : class, IRunnable, new()
    {
        return new T();
    }

    public void Run()
    {
        Console.WriteLine("hello world");
    }
}

interface IRunnable
{
    public void Run();
}