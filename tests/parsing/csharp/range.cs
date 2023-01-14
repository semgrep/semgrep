using System;

class HelloWorldRange
{
    public static void Main()
    {
        string foo = "hello hello world world";
        Console.WriteLine(foo[6..17]);
        Console.WriteLine(foo[..5]);
        Console.WriteLine(foo[18..]);
        Console.WriteLine(foo[6..^6]);
        
        var end = ^6;
        Console.WriteLine(foo[6..end]);
        Console.WriteLine(foo[new System.Index(6)..new System.Index(6, true)]);
        Console.WriteLine(foo[6..new System.Index(6, true)]);
    }
}