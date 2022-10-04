object Foo {
    def foo () : Int = {
        //ERROR:
        if (cond) {
            return 1
        }
    }
}