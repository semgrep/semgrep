package test

object MatchTaintTuple2 {
  def source(): String = "tainted"
  def sink(x: String): Unit = {}

  def test2(triple: (Int, String, Boolean)): Unit = {
    val tainted = source()
    val tup = (1, tainted, true)

    tup match {
      case (a, b, c) =>
        // todoproruleid: taint-match-tuple
        sink(b)
    }
  }
}
