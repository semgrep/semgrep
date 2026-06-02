// C# 14: `nameof` with unbound generic types.
// https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-14.0/nameof-unbound-generic-types

using System.Collections.Generic;

public static class Names
{
    // Unbound generic type names (no type arguments).
    public static readonly string ListName = nameof(List<>);
    public static readonly string MapName = nameof(Dictionary<,>);

    // Compare with closed generic forms, both still legal.
    public static readonly string OpenListName = nameof(List<int>);
    public static readonly string OpenMapName = nameof(Dictionary<string, object>);

    // Unbound nested generic name.
    public static readonly string EnumeratorName = nameof(List<>.Enumerator);
}
