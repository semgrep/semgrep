// C# 11: raw string literals (triple-quoted) and raw interpolated strings.
// https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/raw-string

using System;

public class RawStrings
{
    public static void Main()
    {
        string sql = """
            SELECT *
            FROM "Users"
            WHERE Active = true
        """;

        string json = """
            {
                "name": "value",
                "nested": { "x": 1 }
            }
        """;

        // More-than-three quotes when the content itself contains """
        string code = """""
            string s = """raw inside raw""";
        """"";

        // Raw interpolated string.
        string name = "World";
        string greeting = $"""
            Hello, {name}!
            Path: C:\Users\{name}
        """;

        // u8 suffix for UTF-8 byte string literal (C# 11).
        ReadOnlySpan<byte> bytes = """
            hello
        """u8;

        Console.WriteLine(sql);
        Console.WriteLine(json);
        Console.WriteLine(code);
        Console.WriteLine(greeting);
        Console.WriteLine(bytes.Length);
    }
}
