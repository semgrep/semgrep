package test

object MatchTaintLiteral2 {
  def source(): Int = 42
  def sink(x: Int): Unit = {}

  def test2(code: Int): Unit = {
    val tainted = source()

    code match {
      case 1 => ()
      case 2 =>
        // todoproruleid: taint-match-literal
        sink(tainted)
      case 3 => ()
      case _ => ()
    }
  }
}
