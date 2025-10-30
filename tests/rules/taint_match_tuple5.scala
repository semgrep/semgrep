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
        // protodook: taint-match-tuple
        sink(x)
        // proruleid: taint-match-tuple
        sink(y)
    }
  }
}
