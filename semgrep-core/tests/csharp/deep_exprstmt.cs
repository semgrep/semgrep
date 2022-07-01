using System;

class HelloWorldFuncCall
{
    public static string Test()
    {
        //ERROR:
        foo();
        bar();

        //ERROR:
        foo();
        Console.WriteLine(bar());

        //ERROR:
        foo();
        var x = bar();

	//ERROR:
        foo();
	return bar();
    }

    private static string bar()
    {
        return "hello world";
    }
}
