using System;

class HelloWorldForeach
{
    public static void Main()
    {
        var words = new string[] {"hello", "world"};
        foreach (var word in words)
        {
            Console.WriteLine(word);
        }
    }
}