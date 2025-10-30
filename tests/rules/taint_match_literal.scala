package test

object MatchTaintLiteral {
  def source(): Int = 42
  def sink(x: Int): Unit = {}

  def test1(status: Int): Unit = {
    val tainted = source()

    status match {
      case 200 =>
        // todoproruleid: taint-match-literal
        sink(tainted)
      case 404 => ()
      case _ => ()
    }
  }
}
