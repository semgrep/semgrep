// C# 11: required members must be initialized by the caller.
// https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/required

using System.Diagnostics.CodeAnalysis;

public class Order
{
    public required string CustomerId { get; init; }
    public required decimal Amount { get; set; }
    public string Notes { get; init; }

    [SetsRequiredMembers]
    public Order(string customer, decimal amount)
    {
        CustomerId = customer;
        Amount = amount;
    }

    public Order() {}
}

public class Usage
{
    public static Order BuildOrder()
    {
        return new Order
        {
            CustomerId = "C-001",
            Amount = 19.99m,
        };
    }
}
