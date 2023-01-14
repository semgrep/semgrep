using System;
using System.Globalization;
using System.Linq;

class HelloWorldLinq
{
    public static void Main()
    {
        FromLetSelect();
        DoubleFrom();
        OrderBy();
        Join();
        GroupJoin();
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

    private static void OrderBy()
    {
        var words = new string[] {"hello", "world"};

        var wordList = from w in words
            orderby w.Length descending, w
            select w;

        foreach (var word in wordList)
        {
            Console.WriteLine(word);
        }
    }
    
    private static void Join()
    {
        var words = new string[] {"hello", "world"};

        var wordList = from w1 in words
            join w2 in words on w1 equals w2
            select w2;

        foreach (var word in wordList)
        {
            Console.WriteLine(word);
        }
    }

    private static void GroupJoin()
    {
        var words = new string[] {"hello", "world"};

        var wordList = from w1 in words
            join w2 in words on w1 equals w2 into grouped
            select grouped.Count();

        foreach (var word in wordList)
        {
            Console.WriteLine(word);
        }
    }
}