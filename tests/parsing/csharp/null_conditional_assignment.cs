// C# 14: null-conditional assignment (`?.=` member, `?[…]=` element).
// https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-14.0/null-conditional-assignment

public class Customer
{
    public string Name { get; set; }
    public Customer Manager { get; set; }
    public string[] Tags { get; set; }
}

public class NullCondAssign
{
    public static void Update(Customer c)
    {
        // Member null-conditional assignment.
        c?.Name = "anon";

        // Chained null-conditional with assignment.
        c?.Manager?.Name = "boss";

        // Element null-conditional assignment.
        c?.Tags?[0] = "primary";

        // Compound forms.
        c?.Name += " (updated)";
        c?.Tags?[1] ??= "fallback";
    }
}
