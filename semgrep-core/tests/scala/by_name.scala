object Foo {
    //ERROR:
    def by_name (f : (=> Int) => Int) : Int = f(0)

    //OK:
    def by_value (f : Int => Int) : Int = f(0)
}
