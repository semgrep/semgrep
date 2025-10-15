object Test extends App {
  val number = 1

  number match {
    // MATCH:
    case 1 => println("MATCH")
    // OK:
    case _ => println("OK")
  }
}
