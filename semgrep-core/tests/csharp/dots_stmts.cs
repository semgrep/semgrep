public static void Foo()
{
    //ERROR:
    userData = Get();
    Console.WriteLine("do stuff");
    FooBar();
    Eval(userData);
    FooBar();
}
