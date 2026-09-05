// C# 14: user-defined compound-assignment operator overloads, plus
// instance increment / decrement operators.
// https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-14.0/user-defined-compound-assignment

public struct Counter
{
    public int Value;

    public Counter(int value) => Value = value;

    // Compound-assignment overloads.
    public static void operator +=(ref Counter c, int n) => c.Value += n;
    public static void operator -=(ref Counter c, int n) => c.Value -= n;
    public static void operator *=(ref Counter c, int n) => c.Value *= n;
    public static void operator /=(ref Counter c, int n) => c.Value /= n;
    public static void operator %=(ref Counter c, int n) => c.Value %= n;
    public static void operator &=(ref Counter c, int n) => c.Value &= n;
    public static void operator |=(ref Counter c, int n) => c.Value |= n;
    public static void operator ^=(ref Counter c, int n) => c.Value ^= n;
    public static void operator <<=(ref Counter c, int n) => c.Value <<= n;
    public static void operator >>=(ref Counter c, int n) => c.Value >>= n;
    public static void operator >>>=(ref Counter c, int n) => c.Value >>>= n;

    // Instance increment / decrement operators.
    public void operator ++() => Value++;
    public void operator --() => Value--;
}
