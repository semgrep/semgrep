package test

object MatchTaintConstructor2 {
  def source(): String = "tainted"
  def sink(x: String): Unit = {}

  def test2(opt: Option[String]): Unit = {
    opt match {
      case Some(x) =>
        val tainted = source()
        // ruleid: taint-match-constructor
        sink(tainted)
      case None => ()
    }
  }
}
