// C# 12: lambda parameters with default values and `params` collections.
// https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-12.0/lambda-method-group-defaults

using System;

public class LambdaDefaults
{
    public delegate int IntF(int x);
    public delegate int Combine(int a, int b);
    public delegate string Joiner(params string[] parts);

    public static void Demo()
    {
        // Explicit-typed parameter with default value.
        IntF square = (int x = 5) => x * x;

        // Simple-lambda parameter with default value (no type).
        IntF cube = (x = 2) => x * x * x;

        // Multi-parameter with one default.
        Combine add = (int a, int b = 10) => a + b;

        // params collection parameter inside a lambda.
        Joiner csv = (params string[] parts) => string.Join(",", parts);

        Console.WriteLine(square(0));
        Console.WriteLine(cube(0));
        Console.WriteLine(add(1, 2));
        Console.WriteLine(csv("a", "b", "c"));
    }
}
