// C# 12: inline arrays — a fixed-size buffer expressed as a struct with
// a single field marked `[InlineArray(N)]`.
// https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/inline-arrays

using System;
using System.Runtime.CompilerServices;

[InlineArray(8)]
public struct ByteBuf8
{
    private byte _first;
}

[InlineArray(4)]
public struct Vec4<T>
{
    private T _e0;
}

public static class InlineArraysDemo
{
    public static void Fill(ref ByteBuf8 buf)
    {
        for (int i = 0; i < 8; i++)
        {
            buf[i] = (byte)i;
        }
    }

    public static int Sum(ref ByteBuf8 buf)
    {
        int total = 0;
        foreach (byte b in buf)
        {
            total += b;
        }
        return total;
    }

    public static void Demo()
    {
        ByteBuf8 buf = default;
        Fill(ref buf);
        Console.WriteLine(Sum(ref buf));

        Vec4<int> v = default;
        v[0] = 1;
        v[3] = 4;
        Console.WriteLine(v[0] + v[3]);
    }
}
