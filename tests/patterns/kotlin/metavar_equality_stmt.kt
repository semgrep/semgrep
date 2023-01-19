class Foo {
    fun foo() {
        // ERROR:
        if (x > 2) {
            bar()
            foo()
        } else {
            bar()
            foo()
        }
   
        if (x > 2) {
            foo()
            bar()
        } else {
            foo()
        }
    }
}
