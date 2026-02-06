// Test for ::class and ::method patterns (Issue #11252)
fun main() {
    // Class reference with ::class.java
    val clazz1 = BigDecimal::class.java
    val clazz2 = String::class

    // Method reference
    val func = MyClass::myMethod

    // Used in function call
    assertInstanceOf(BigDecimal::class.java, cell)
}
