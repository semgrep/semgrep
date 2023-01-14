using System;

public class HelloWorldGoto
{
    public static void Main()
    {
        goto hello;
        world:
        Console.WriteLine("World");
        goto end;
        
        hello:
        Console.WriteLine("Hello");
        goto world;
        
        end: ;
    }
}