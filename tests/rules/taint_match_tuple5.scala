package test

object MatchTaintTuple5 {
  def source(): String = "tainted"
  def sink(x: String): Unit = {}

  def testIndexSensitivity(): Unit = {
    val tainted = source()
    val safe = "safe"
    val tup = (safe, tainted)

    tup match {
      case (x, y) =>
        // ok: taint-match-tuple
        sink(x)
        // todoproruleid: taint-match-tuple
        sink(y)
    }
  }
}
