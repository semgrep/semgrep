using System;
using System.Globalization;
using System.Linq;

class HelloWorldLinq
{
    public static void Main()
    {
        FromLetSelect();
        DoubleFrom();
    }

    private static void FromLetSelect() {
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

    public static void DoubleFrom()
    {
        var words1 = new string[] {"hello", "world"};

        var wordList = from w1 in words1
            from w2 in w1
            select w1 + w2;

        foreach (var word in wordList)
        {
            Console.WriteLine(word);
        }
    }
}