using System;

class HelloWorldFuncCall
{
    public static string Test()
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

	//ERROR:
	return StaticMethod();
    }

    private static string StaticMethod()
    {
        return "hello world";
    }
}
