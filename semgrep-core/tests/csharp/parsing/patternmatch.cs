using System;

class HelloWorldPattern
{
    public static void Main()
    {
        for (int i = 1; i < 20; i++)
        {
            var text = (i % 3, i % 5) switch
            {
                (0, 0) => "FizzBuzz",
                (0, _) => "Fizz",
                (_, 0) => "Buzz",
                (_, _) => i.ToString()
            };
            Console.WriteLine(text);
        }
    }
}