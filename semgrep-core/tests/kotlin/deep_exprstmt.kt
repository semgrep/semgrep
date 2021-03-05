fun foo() {
    //ERROR: match
    bar()
    //ERROR: match
    val x = bar()
    //ERROR: match
    foo2(bar())
    //ERROR: match
    return bar()
}
  
