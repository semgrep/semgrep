object Foo {

    //ERROR:
    def f(x : Int) : Int = {
        val y = 0
        case class F(x : Int)
        x
    }    

    //OK:
    def g(x : Int) : Int = {
        val y = 0
        x
    }
}