package test

object MatchTaintConstructor3 {
  def source(): String = "tainted"
  def sink(x: String): Unit = {}

  def test3Safe(opt: Option[String]): Unit = {
    val safe = "safe"
    val wrapped = Some(safe)

    wrapped match {
      // ok: taint-match-constructor
      case Some(x) => sink(x)
      case None => ()
    }
  }
}
