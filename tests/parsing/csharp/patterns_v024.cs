unsafe ref partial struct S
{
    int P(int* p, long* q)
    {
        int x = *(p + 1);
        long y = *(long*)q;
        return x + (int)y;
    }
}

class C
{
    const int Max = 10;

    string Classify(int[] xs, int n) => xs switch
    {
        [var a] when a > 0 => "one positive",
        [_, .., _] => "two or more",
        [..] => "any",
    };

    bool IsMax(int n) => n is checked(Max * 2);

    void Lambda()
    {
        var f = (in int x) => x;
        var g = (out int y) => { y = 1; };
    }
}
