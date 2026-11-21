class C
{
    void Foo(Obj? o)
    {
        // ERROR:
        o?.Name = "anon";

        // ERROR:
        o?.Other = ComputeValue();

        // not an assignment — should not match
        var n = o?.Name;
    }
}
