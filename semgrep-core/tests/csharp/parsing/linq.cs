using System;
using System.Globalization;
using System.Linq;

class HelloWorldLinq
{
    public static void Main()
    {
        var textInfo = new CultureInfo("en-US", false).TextInfo;

        var words = new string[] {"hello", "world"};
        var greeting = 
            from word in words
            let title = textInfo.ToTitleCase(word)
            select word + title;

        foreach (string w in greeting)
        {
            Console.WriteLine(w);
        }
    }
}