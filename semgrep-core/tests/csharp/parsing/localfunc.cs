using System;

class HelloWorldLocalFunc
{
    public static void Main()
    {
        void Hello() => SomeMethod();
        
        Hello();
        World();

        void World()
        {
            Console.WriteLine("world");
        }
    }

    private static void SomeMethod()
    {
        Console.WriteLine("hello");
    }
}