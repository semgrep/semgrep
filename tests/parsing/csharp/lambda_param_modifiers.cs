// C# 14: simple-lambda parameter modifiers (no explicit parameter type
// required).
// https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-14.0/simple-lambda-parameter-modifiers

using System;

public class LambdaModifiers
{
    public delegate void OutAction(out int x);
    public delegate void RefAction(ref int x);
    public delegate void InAction(in int x);
    public delegate void ScopedSpan(scoped Span<int> s);

    public static void Demo()
    {
        OutAction o = (out v) => v = 7;
        RefAction r = (ref v) => v++;
        InAction i = (in v) => Console.WriteLine(v);
        ScopedSpan s = (scoped Span<int> span) => Console.WriteLine(span.Length);

        int x = 0;
        o(out x);
        r(ref x);
        i(in x);
    }
}
