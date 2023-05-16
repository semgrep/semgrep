// MATCH:
try {
    val result = 10 / 0 // division by zero
} catch {
    case e: Exception =>
        println("handled") // code to handle the exception
}