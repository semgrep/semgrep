class C
{
    void Foo(Obj? o)
    {
        // ERROR:
        var x = o?.Name;

        // ERROR:
        Use(o?.Value);

        var plain = o.Name;
    }
}
