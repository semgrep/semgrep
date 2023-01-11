object Test {
  def print(): String = {
    var XXX = 1
    s"$$XXX"
  }
    def main(args: Array[String]): Unit = {
      println(print())
   }
}
