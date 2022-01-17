object Test {
    // ERROR:
    def foo() : Int = 1

    // ERROR:
    def bar(bar1 : Int, bar2 : Bool, bar3 : String) : Bool = {
        return bar2
    }

}