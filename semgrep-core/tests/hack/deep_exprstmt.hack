class Foo
{
    public function foo()
    {
        //ERROR: match
        bar();
        //ERROR: match
        $x = bar();
        //ERROR: match
        foo2(bar());
        //ERROR: match
        return bar();
    }
}
