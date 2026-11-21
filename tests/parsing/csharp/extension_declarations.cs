// C# 14: extension declarations
// https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-14.0/extensions

using System;

public static class Enumerables
{
    extension<T>(IEnumerable<T> source)
    {
        public bool IsEmpty => !source.Any();

        public T FirstOrDefaultIfEmpty(T fallback)
        {
            return source.Any() ? source.First() : fallback;
        }
    }

    extension(string s)
    {
        public string TrimQuotes() => s.Trim('"');
    }
}

public class Demo
{
    public static void Main()
    {
        var xs = new[] { 1, 2, 3 };
        Console.WriteLine(xs.IsEmpty);
        Console.WriteLine("\"hi\"".TrimQuotes());
    }
}
