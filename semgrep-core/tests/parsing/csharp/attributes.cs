// https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/attributes/

using System;
using System.Reflection;
[assembly: AssemblyTitleAttribute("Production assembly 4")]
[module: CLSCompliant(true)]

[Serializable]
public class Foo
{
    // default: applies to method
    [ValidatedContract]
    int Method1() { return 0; }

    // applies to method
    [method: ValidatedContract]
    int Method2() { return 0; }

    // applies to parameter
    int Method3([ValidatedContract] string contract) { return 0; }

    // applies to return value
    [return: ValidatedContract]
    int Method4() { return 0; }
}

