fun test() {
    //ERROR:
    assertInstanceOf(BigDecimal::class.java, cell)

    //ERROR:
    assertInstanceOf(String::class.java, name)

    // Not a match (different function)
    checkType(BigDecimal::class.java, cell)
}
