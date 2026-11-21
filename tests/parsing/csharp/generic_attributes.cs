// C# 11: generic attributes.
// https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-11.0/generic-attributes

using System;

[AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, AllowMultiple = true)]
public class TypeAttribute<T> : Attribute
{
    public Type Wrapped => typeof(T);
}

[AttributeUsage(AttributeTargets.Method)]
public class ImplementedByAttribute<TInterface, TImplementation> : Attribute
    where TImplementation : TInterface
{
}

[TypeAttribute<int>]
[TypeAttribute<string>]
public class Decorated
{
    [ImplementedByAttribute<IComparable<int>, int>]
    public int Compare(int a, int b) => a.CompareTo(b);
}
