package test

object MatchTaintTuple4 {
  def source(): String = "tainted"
  def sink(x: String): Unit = {}

  def test4Safe(pair: (String, Int)): Unit = {
    val safe = "safe"
    val tup = (safe, 42)

    tup match {
      // ok: taint-match-tuple
      case (x, y) => sink(x)
    }
  }
}
