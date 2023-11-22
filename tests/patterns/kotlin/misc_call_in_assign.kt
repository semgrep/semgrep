package annotation.test


public object TestHce {
    fun f1(context : Context) {
        // MATCH: 
        Foo(context).elem = var1
    }

    fun f2() {
        // MATCH: 
        Bar().elem = var2
    }

    public fun f3(context: Context): Int {
        return 0
    }
}