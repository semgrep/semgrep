package test

object MatchTaintTuple3 {
  def source(): String = "tainted"
  def sink(x: String): Unit = {}

  def test3Nested(nested: ((String, Int), Boolean)): Unit = {
    val tainted = source()
    val tup = ((tainted, 42), true)

    tup match {
      case ((x, y), z) =>
        // proruleid: taint-match-tuple
        sink(x)
    }
  }
}
