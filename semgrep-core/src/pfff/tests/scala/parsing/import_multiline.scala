import scala.util.{
  Try,
  Success,
  Failure,
}
object Test {
    def divideXByY(x: Int, y: Int): Try[Int] = {
      Try(x / y)
    }
    def print(): String = {
      val UsdSymbol = "$"
      s"${divideXByY(1,1).getOrElse("")}"
    }
    def main(args: Array[String]): Unit = {
      println(print())
   }
}
