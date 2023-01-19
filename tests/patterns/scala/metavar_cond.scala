object Foo {
    def f(x : Bool) : Int = {
        //ERROR:
        if (x) {
            return foo()
        }
    }
}