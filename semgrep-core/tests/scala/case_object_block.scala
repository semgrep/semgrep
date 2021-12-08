object Foo {

    //ERROR:
    def f(x : Int) : Int = {
        val y = 0
        case object F
        x
    }    

    //OK:
    def g(x : Int) : Int = {
        val y = 0
        x
    }
}