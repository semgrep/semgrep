import scala.util.{Try,Success,Failure}
object Test {
    def divideXByY(x: Int, y: Int): Try[Int] = {
      Try(x / y)
    }
  def print(): String = {
      val UsdSymbol = "$"
    // According to official grammar, this should be a parse error
    // because UsdSymbol$ is a valid identifier ($ is ok in id)
    // but the Scala compilers seems to parse it correctly ...
      s" $UsdSymbol${divideXByY(1,0).getOrElse("")}"
    }
    def main(args: Array[String]): Unit = {
      println(print())
   }
}
