// C# 12: `ref readonly` parameters in addition to `in`, `ref`, `out`.
// https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-12.0/ref-readonly-parameters

using System;

public readonly struct Buffer
{
    public readonly int Length;
    public Buffer(int length) => Length = length;
}

public static class ParamModes
{
    // `ref readonly` accepts the parameter by reference but disallows
    // writes — like `in`, but the caller is expected to pass an
    // explicit `ref` (or the compiler emits a warning).
    public static int SumHeader(ref readonly Buffer head)
    {
        return head.Length;
    }

    public static void Compose(in int a, ref int b, out int c, ref readonly Buffer d)
    {
        c = a + b + d.Length;
        b += 1;
    }

    public static void Demo()
    {
        var buf = new Buffer(16);
        Console.WriteLine(SumHeader(ref buf));

        int sum = 0;
        int total = 0;
        Compose(in sum, ref total, out var result, ref buf);
        Console.WriteLine(result);
    }
}
