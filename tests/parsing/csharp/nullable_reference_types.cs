// C# 8+: nullable reference types (NRT) — `string?`, the `!` null-forgiving
// operator, `??`/`??=`/`?.`, nullable generic constraints, the
// `#nullable enable`/`disable`/`restore` pragmas, and the flow-analysis
// attributes from `System.Diagnostics.CodeAnalysis`.
// https://learn.microsoft.com/en-us/dotnet/csharp/nullable-references

#nullable enable

using System;
using System.Diagnostics.CodeAnalysis;

public class Container<T> where T : class?
{
    // Nullable reference fields and properties.
    public T? Value;
    public string? Name { get; set; }

    // Non-nullable property initialized via the null-forgiving operator —
    // a common pattern at field declarations.
    public string Required { get; set; } = null!;

    [return: NotNull]
    public T GetOrThrow() => Value ?? throw new InvalidOperationException();

    // Flow-analysis attributes.
    [MemberNotNullWhen(true, nameof(Name))]
    public bool HasName() => Name is not null;

    public bool TryGetName([NotNullWhen(true)] out string? name)
    {
        name = Name;
        return name is not null;
    }

    [MemberNotNull(nameof(Name))]
    public void EnsureName(string fallback)
    {
        Name ??= fallback;
    }

    public string Describe()
    {
        string? maybe = Name;
        maybe ??= "(none)";
        return maybe!;
    }
}

public static class NullPatterns
{
    public static string Classify(object? o) =>
        o switch
        {
            null => "null",
            string s when s.Length == 0 => "empty",
            string => "non-empty string",
            not null => "other non-null",
        };

    public static int Length(string? s)
    {
        // Throw if null, then dereference safely with `!`.
        _ = s ?? throw new ArgumentNullException(nameof(s));
        return s!.Length;
    }

    public static T? Pick<T>(T? a, T? b) where T : class? => a ?? b;
}

// Generic constraints that mention nullability explicitly.
public class StrictBox<T> where T : notnull
{
    public T Value { get; }
    public StrictBox(T value) => Value = value;
}

public class MaybeBox<T> where T : class?
{
    public T? Value { get; init; }

    public void Mutate([DisallowNull] ref T? v) => v = v;

    [return: MaybeNull]
    public T Get() => Value;

    public bool TryGet([MaybeNullWhen(false)] out T value)
    {
        value = Value!;
        return Value is not null;
    }
}

#nullable disable

// Inside a `#nullable disable` region the compiler does not enforce
// nullable annotations on reference types; semgrep should still parse
// these forms cleanly.
public class Untracked
{
    public string Field;
    public string Describe(string s) => s + Field;
}

#nullable restore

public class Restored
{
    public string? AfterRestore { get; set; }
}
