using System;

class HelloWorldFor
{
    private int _index = 0;

    public static void Main()
    {
        new HelloWorldFor().Greet();
    }

    private void Greet() {
       var greeting = "hello world";
        for (_index = 0; _index < greeting.Length; _index++)
        {
            Console.Write(greeting[_index]);
        }

        _index = 0;
        for (;_index < greeting.Length; _index++)
        {
            Console.Write(greeting[_index]);
        }

        var length = 0;
        for (_index = 0, length = greeting.Length;
            _index < greeting.Length; 
            _index++, length--)
        {
            Console.Write(greeting[_index]);
        }
    }
}