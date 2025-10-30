package test

object MatchTaintTuple {
  def source(): String = "tainted"
  def sink(x: String): Unit = {}

  def test1(pair: (String, Int)): Unit = {
    val tainted = source()
    val tup = (tainted, 42)

    tup match {
      case (x, y) =>
        // proruleid: taint-match-tuple
        sink(x)
    }
  }
}
