using System;

class HelloWorldTernary
{
    public static void Main()
    {
        var hello = new SpaceString("hello");
        var world = new SpaceString("world");
        Console.WriteLine(hello + world);
        Console.WriteLine((int)hello);
    }
}

internal class SpaceString
{
    private string content;

    public SpaceString(string content)
    {
        this.content = content;
    }

    public static SpaceString operator +(SpaceString a, SpaceString b)
    {
        return new SpaceString(a.content + " " + b.content);
    }

    public static implicit operator string(SpaceString s)
    {
        return s.content;
    }
    
    public static explicit operator int(SpaceString s)
    {
        return s.content.Length;
    }
}