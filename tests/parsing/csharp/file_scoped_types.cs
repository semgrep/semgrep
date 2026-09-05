// C# 11: file-scoped types. The `file` modifier restricts visibility
// to the source file in which the type is declared.
// https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/file

namespace MyApp.Internals;

file class Helper
{
    public static int Compute(int x) => x * x + 1;
}

file struct Buf
{
    public int Length;
    public int Capacity;
}

file interface IInternal
{
    void Run();
}

file delegate void Action0();

file enum Mode { On, Off, Auto }

public class PublicSurface
{
    public static int Use(int x) => Helper.Compute(x);
}
