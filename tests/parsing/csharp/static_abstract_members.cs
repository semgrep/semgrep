// C# 11: static abstract / virtual members in interfaces (generic math).
// https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/tutorials/static-virtual-interface-members

using System;

public interface IAddable<T> where T : IAddable<T>
{
    static abstract T operator +(T a, T b);
    static abstract T Zero { get; }
}

public interface IMonoid<T> : IAddable<T> where T : IMonoid<T>
{
    static virtual T Identity => T.Zero;
}

public readonly struct Int32Sum : IMonoid<Int32Sum>
{
    public int Value { get; }
    public Int32Sum(int value) => Value = value;

    public static Int32Sum operator +(Int32Sum a, Int32Sum b) =>
        new(a.Value + b.Value);

    public static Int32Sum Zero => new(0);
}

public static class Reducer
{
    public static T Sum<T>(System.Collections.Generic.IEnumerable<T> items)
        where T : IAddable<T>
    {
        T total = T.Zero;
        foreach (var x in items)
        {
            total = total + x;
        }
        return total;
    }
}
