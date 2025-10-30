package test

object MatchTaintLiteral4 {
  def source(): Int = 42
  def sink(x: Int): Unit = {}

  def test4Safe(status: Int): Unit = {
    val safe = 100

    status match {
      case 200 =>
        // ok: taint-match-literal
        sink(safe)
      case _ => ()
    }
  }
}
