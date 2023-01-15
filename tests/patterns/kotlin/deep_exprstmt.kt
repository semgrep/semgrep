fun foo() {
    //ERROR: match
    foo()
    bar()
    //ERROR: match
    foo()
    val x = bar()
    //ERROR: match
    foo()
    foo2(bar())
    //ERROR: match
    foo()
    return bar()
}
  
