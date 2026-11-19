class C
{
    int[] Items = [
        1,
#if DEBUG
        2,
        3,
#endif
        4,
    ];

    int[] Init = {
        1,
#if DEBUG
        2,
#endif
        3
    };

    void M()
#if NET8_0
    {
        Console.WriteLine("net8");
    }
#else
        => Console.WriteLine("other");
#endif

    int R(bool c, int a, int b)
    {
        return c ? a :
#if DEBUG
            b;
#else
            0;
#endif
    }

    void E(bool a, bool b)
    {
        if (a) { Foo(); }
#if DEBUG
        else if (b) { Bar(); }
#endif
        else { Baz(); }
    }
}
