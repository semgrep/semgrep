// C# 12: primary constructors on classes and structs.
// https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/tutorials/primary-constructors

using System;

// Class with a primary constructor that captures `name` and `id`.
public class Person(string name, int id)
{
    public string Name => name;
    public int Id { get; init; } = id;

    public override string ToString() => $"{Name}#{Id}";
}

// Struct with a primary constructor and a computed property.
public struct Vector2(double dx, double dy)
{
    public double Dx => dx;
    public double Dy => dy;
    public double Magnitude => Math.Sqrt(dx * dx + dy * dy);

    public Vector2 Scale(double factor) => new(dx * factor, dy * factor);
}

// Primary constructor forwarding to a base class via `: base(...)`.
public abstract class Animal(string species)
{
    public string Species { get; } = species;
}

public class Dog(string name) : Animal("Canis lupus familiaris")
{
    public string Name => name;
}

// Generic class with a primary constructor.
public sealed class Box<T>(T initial)
{
    private T _value = initial;

    public T Get() => _value;
    public void Set(T value) => _value = value;
}

// Record with primary constructor (already supported in C# 9 but
// covered here together with the new class form).
public record class Point(int X, int Y);

// Record struct with primary constructor.
public record struct PixelRgb(byte R, byte G, byte B);

public static class Demo
{
    public static void Main()
    {
        var p = new Person("Ada", 1);
        Console.WriteLine(p);

        var v = new Vector2(3, 4);
        Console.WriteLine(v.Magnitude);

        var d = new Dog("Rex");
        Console.WriteLine($"{d.Name} is a {d.Species}");

        var b = new Box<int>(42);
        b.Set(b.Get() + 1);
        Console.WriteLine(b.Get());

        Console.WriteLine(new Point(2, 3));
        Console.WriteLine(new PixelRgb(255, 128, 0));
    }
}
