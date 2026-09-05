// C# 14: field-backed properties using the contextual `field` keyword.
// https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-14.0/field-keyword

public class Person
{
    // Auto-property whose backing field is referenced via `field`.
    public string Name
    {
        get => field;
        set => field = value ?? "(none)";
    }

    // Trim-on-set behavior using `field`.
    public string DisplayName
    {
        get;
        set => field = (value ?? string.Empty).Trim();
    }

    // `field` works inside getters too, alongside a default initializer.
    public int Score
    {
        get => field >= 0 ? field : 0;
        set;
    } = 10;
}
