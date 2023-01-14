using System;

class HelloWorldLock
{
    private static object _lockObject = new Object();

    public static void Main()
    {
        lock (_lockObject)
        {
            Console.WriteLine("hello world");
        }
    }
}