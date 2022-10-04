
struct S1 { public int x; }

public class Foo
{
    int M1(S1 s) { return s.x; }

    int M2() { return M1(new () { x = 42 }); }

}

