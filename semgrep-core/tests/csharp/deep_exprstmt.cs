using System;

class HelloWorldFuncCall
{
    public static void Main()
    {
        //ERROR:
        StaticMethod();

        //ERROR:
        Console.WriteLine(StaticMethod());

        //ERROR:
        var x = StaticMethod();

        //ERROR:
        if (null == StaticMethod())
        {
            //ERROR:
            throw new NullReferenceException(StaticMethod());
        }
    }

    private static string StaticMethod()
    {
        return "hello world";
    }
}