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

    /**
     * @param context your Application or Activity context
     */
    public fun f3(context: Context): Int {
        return 0
    }

    /**
     * @param context your Application or Activity context
     * @return returns currently used theme resource id
     */
    @StyleRes
    public fun f4(context: Context) {
        return 0
    }
}