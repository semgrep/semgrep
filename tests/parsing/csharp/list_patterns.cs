// C# 11: list patterns, including slice patterns inside them.
// https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/patterns#list-patterns

public class ListPatterns
{
    public static string Classify(int[] xs)
    {
        return xs switch
        {
            [] => "empty",
            [_] => "singleton",
            [1, 2, 3] => "exact triple",
            [var head, .. var tail] => $"head={head}, n={tail.Length + 1}",
            _ => "other",
        };
    }

    public static bool StartsAndEndsWith(string[] xs, string first, string last)
    {
        return xs is [var f, .., var l] && f == first && l == last;
    }

    public static bool Bracketed(int[] xs)
    {
        return xs is [1, .., 9];
    }
}
