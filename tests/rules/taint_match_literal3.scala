package test

object MatchTaintLiteral3 {
  def source(): Int = 42
  def sink(x: Int): Unit = {}

  def test3WithGuard(x: Int, y: Int): Unit = {
    val tainted = source()

    x match {
      case 1 if y > 0 =>
        // ruleid: taint-match-literal
        sink(tainted)
      case 2 => ()
      case 3 =>
        // ruleid: taint-match-literal
        sink(tainted)
      case _ => ()
    }
  }
}
