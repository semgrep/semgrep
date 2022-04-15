object Foo {
    def foo() = {

        //ERROR:
        if (cond) {
            bar()
        }
        else {
            bar()
        }

        if (cond) {
            bar()
        }
        else {
            baz()
        }

    }
}