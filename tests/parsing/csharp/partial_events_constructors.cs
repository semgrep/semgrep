// C# 14: partial events and partial constructors
// https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-14.0/partial-events-and-constructors

using System;

public partial class Service
{
    public partial event EventHandler<int> WorkCompleted;

    public partial Service(string name);
}

public partial class Service
{
    private readonly string _name;

    public partial event EventHandler<int> WorkCompleted
    {
        add { Console.WriteLine($"+ subscribe {_name}"); }
        remove { Console.WriteLine($"- unsubscribe {_name}"); }
    }

    public partial Service(string name)
    {
        _name = name;
    }
}
